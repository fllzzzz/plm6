<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-button type="primary" @click="formVisible=true" v-permission="permission.add">添加</common-button>
    <common-table
    ref="tableRef"
    :data="tableData"
    empty-text="暂无数据"
    :max-height="maxHeight"
    return-source-data
    :showEmptySymbol="false"
    :stripe="false"
    style="width: 100%;margin-top:10px;"
    :cell-class-name="wrongCellMask"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="*代表杆件类型" align="center">
      <template v-slot="scope">
        <el-input v-if="scope.row.isModify" v-model.trim="scope.row.name" type="text" placeholder="代表杆件类型" maxlength="30" />
        <span v-else>{{ scope.row.name }}</span>
      </template>
    </el-table-column>
    <el-table-column key="specPrefix" prop="specPrefix" label="*组立号规格前缀(大写)" align="center">
      <template v-slot="scope">
        <el-input v-if="scope.row.isModify" v-model.trim="scope.row.specPrefix" type="text" placeholder="请填写大写字母" maxlength="10" @blur="getName(scope.row,scope.$index)"/>
        <span v-else>{{ scope.row.specPrefix }}</span>
      </template>
    </el-table-column>
    <el-table-column key="boolSchedulingEnum" prop="boolSchedulingEnum" label="*是否有生成工序" align="center">
      <template v-slot="scope">
        <common-radio v-if="scope.row.isModify" v-model="scope.row.boolSchedulingEnum" :options="whetherEnum.ENUM" type="enum" />
        <span v-else>{{ scope.row.boolSchedulingEnum ? '√' : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column key="sort" prop="sort" :show-overflow-tooltip="true" label="序号" align="center">
      <template v-slot="scope">
        <el-input-number
          v-if="scope.row.isModify"
          v-model="scope.row.sort"
          placeholder="请填写"
          type="text"
          controls-position="right"
          :min="0"
          :max="10000"
        />
        <div v-else>{{ scope.row.sort }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      label="操作"
      width="160px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <template v-if="scope.row.isModify">
          <common-button type="info" size="mini" @click="rowCancel(scope.row,scope.$index)">取消</common-button>
          <common-button type="primary" plain size="mini" @click="rowSubmit(scope.row)">保存</common-button>
        </template>
        <template v-else>
          <common-button icon="el-icon-edit" type="primary" size="mini" @click="modifyRow(scope.row)" />
          <common-button icon="el-icon-delete" type="danger" size="mini" @click="rowDelete(scope.row)" />
        </template>
      </template>
    </el-table-column>
  </common-table>
  <mForm v-model="formVisible" @success="fetchData"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref } from 'vue'
import { ElMessage } from 'element-plus'
import { machinePartConfigPM as permission } from '@/page-permission/config'
import useMaxHeight from '@compos/use-max-height'
import { whetherEnum } from '@enum-ms/common'
import mForm from './module/form'
import { validate } from '@compos/form/use-table-validate'

const tableRef = ref()
const tableData = ref([])
const formVisible = ref(false)
const originRow = ref({})
const { maxHeight } = useMaxHeight({
  wrapperBox: '.machinePartConfig',
  paginate: true,
  extraHeight: 40
})
// 序号校验
const validateSort = (value, row) => {
  if (!value) return false
  return true
}

const tableRules = {
  name: [{ required: true, message: '请输入杆件类型', trigger: 'blur' }],
  specPrefix: [{ required: true, message: '请输入组立号规格前缀', trigger: 'blur' }],
  boolSchedulingEnum: [{ required: true, message: '请选择是否有生成工序', trigger: 'change' }],
  sort: [{ validator: validateSort, message: '请输入序号', trigger: 'change' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

fetchData()

async function fetchData() {
  try {
    const { content } = await crudApi.get({ productType: 1 })
    tableData.value = content || []
  } catch (e) {
    console.log('特殊零件标记', e)
  }
}

function getName(val, index) {
  if (val.specPrefix && !/^[A-Z]+$/.test(val.specPrefix)) {
    tableData.value[index].specPrefix = undefined
  }
}

function modifyRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
}

async function rowDelete(row) {
  try {
    await crudApi.del([row.id])
    ElMessage.success(`删除成功`)
    fetchData()
  } catch (e) {
    console.log(`删除失败`, e)
  }
}
function rowCancel(row) {
  row.isModify = false
  row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
}

async function rowSubmit(row) {
  const rules = tableRules
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  try {
    await crudApi.edit(row)
    ElMessage.success(`修改成功`)
    fetchData()
  } catch (e) {
    console.log('修改', e)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>

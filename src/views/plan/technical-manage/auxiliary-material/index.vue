<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @currentChange="currentChange"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        @sort-change="crud.handleSortChange"
        class="enclosure-table"
        :cell-class-name="wrongCellMask"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
        <template #default="{ row }">
          <div class="mtb-10">
            <el-input
              v-if="row.isModify"
              v-model="row.remark"
              :rows="1"
              :autosize="{ minRows: 1, maxRows: 1 }"
              type="textarea"
              placeholder="备注"
              maxlength="200"
              show-word-limit
              style="width: 400px"
            />
            <div v-else>
              备注:<span v-empty-text>{{ row.remark }}</span>
            </div>
          </div>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" v-if="columns.visible('serialNumber')" :show-overflow-tooltip="true" />
      <el-table-column
        prop="classifyName"
        label="名称"
        align="center"
        fixed="left"
        min-width="150"
        v-if="columns.visible('classifyName')"
        :show-overflow-tooltip="true"
      >
        <template #default="{ row }">
          <el-tooltip :content="row.classifyFullName" :disabled="!row.classifyFullName" :show-after="200" placement="top">
            <span v-empty-text="row.classifyName" />
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specification')" :show-overflow-tooltip="true" prop="specification" label="规格" align="center" min-width="200px" fixed="left">
        <template #default="{ row }">
          <span v-empty-text="row.specification" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('measureUnit')" :show-overflow-tooltip="true" prop="measureUnit" label="计量单位" align="center" min-width="70px">
        <template #default="{ row }">
          <span v-empty-text>{{ row.measureUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('quantity')" :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.measureUnit && row.isModify"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.measurePrecision"
            size="mini"
            placeholder="数量"
          />
          <span v-else v-empty-text="row.quantity" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('accountingUnit')" :show-overflow-tooltip="true" prop="accountingUnit" label="核算单位" align="center" min-width="70px">
        <template #default="{ row }">
          <span v-empty-text>{{ row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('mete')" :show-overflow-tooltip="true" prop="mete" label="核算量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.isModify"
            v-model="row.mete"
            :min="0.000001"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.accountingPrecision"
            size="mini"
            placeholder="核算量"
          />
          <span v-else v-empty-text>{{ row.mete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('color')" :show-overflow-tooltip="true" prop="color" label="颜色" align="center" min-width="120px">
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.color" maxlength="20" size="mini" placeholder="颜色" />
          <span v-else v-empty-text>{{ row.color }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('brand')" :show-overflow-tooltip="true" prop="brand" label="品牌" align="center" min-width="120px">
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
          <span v-else v-empty-text>{{ row.color }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <template v-if="scope.row.isModify">
            <common-button type="info" size="mini" @click="rowCancel(scope.row)">取消</common-button>
            <common-button type="primary" size="mini" @click="rowSubmit(scope.row)">保存</common-button>
          </template>
          <template v-else>
            <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.row)" />
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="确定删除吗?"
              @confirm="deleteRow(scope.row)"
            >
              <template #reference>
                <common-button size="small" class="el-icon-delete" type="danger"/>
              </template>
            </el-popconfirm>
          </template>
        </template>
      </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-manage/plan-auxiliary-material'
import { provide, ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { ElMessage } from 'element-plus'
import { artifactTreePM as permission } from '@/page-permission/plan'
import { validate } from '@compos/form/use-table-validate'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { positiveNumPattern } from '@/utils/validate/pattern'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const expandRowKeys = ref([]) // 展开行key
const originRow = ref({})
const currentMonomer = ref({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '配套件清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-auxiliary-material',
  paginate: true,
  extraHeight: 40
})

function currentChange(val) {
  currentMonomer.value = val
}

provide('currentMonomer', currentMonomer)
provide('globalProject', globalProject)

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

function editRow(row) {
  if (expandRowKeys.value.indexOf(row.id) < 0) {
    expandRowKeys.value.push(row.id)
  }
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
}
async function deleteRow(row) {
  try {
    await crudApi.del(crud.query.projectId, [row.id])
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('删除', e)
  }
}
function rowCancel(row) {
  if (expandRowKeys.value.indexOf(row.id) > -1) {
    expandRowKeys.value.splice(expandRowKeys.value.findIndex(v => v === row.id), 1)
  }
  row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  row.isModify = false
}

async function rowSubmit(row) {
  const rules = tableRules
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row[rule], row)
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
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.isModify = false
  } catch (e) {
    console.log(`修改`, e)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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

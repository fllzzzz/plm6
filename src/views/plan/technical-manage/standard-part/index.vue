<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @currentChange="currentChange" @currentAreaChange="currentAreaChange"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        return-source-data
        style="width: 100%"
        @sort-change="crud.handleSortChange"
        class="enclosure-table"
        :cell-class-name="wrongCellMask"
        @selection-change="crud.selectionChangeHandler"
      >
      <el-table-column key="selection" type="selection" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column
        prop="name"
        label="名称"
        align="center"
        fixed="left"
        min-width="150"
        v-if="columns.visible('name')"
        :show-overflow-tooltip="true"
      >
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.name" type="text" placeholder="名称" style="width:100%" maxlength="20"/>
          <span v-else>{{row.name}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specification')" :show-overflow-tooltip="true" prop="specification" label="规格" align="center" min-width="200px">
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.specification" type="text" placeholder="规格" style="width:100%" maxlength="20"/>
          <span v-else>{{row.specification}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('measureUnit')" :show-overflow-tooltip="true" prop="measureUnit" label="单位" align="center" min-width="70px" />
      <el-table-column v-if="columns.visible('quantity')" :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.isModify"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            size="mini"
            placeholder="数量"
          />
          <span v-else>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('useProperty')" prop="useProperty" label="使用范围" align="center" min-width="120px">
      <template #default="{ row }">
        <common-select
          v-if="row.isModify"
          v-model="row.useProperty"
          :options="auxiliaryMaterialUseTypeEnum.ENUM"
          type="enum"
          size="small"
          clearable
          placeholder="使用范围"
        />
        <span v-else>{{ auxiliaryMaterialUseTypeEnum.VL[row.useProperty] }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="remark" label="备注" align="center">
      <template #default="{ row }">
        <el-input
          v-if="row.isModify"
          v-model.trim="row.remark"
          type="textarea"
          :autosize="{ minRows: 1, maxRows: 6 }"
          :maxlength="200"
          placeholder="备注"
          style="width:100%"
        />
        <span v-else>{{ row.remark }}</span>
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
            <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.row)" v-permission="permission.edit"/>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="确定删除吗?"
              @confirm="deleteRow(scope.row)"
              v-if="checkPermission(permission.del)"
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
import crudApi from '@/api/plan/technical-manage/standard-part'
import { provide, ref } from 'vue'

import { isNotBlank } from '@data-type/index'
import { validate } from '@compos/form/use-table-validate'
import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { ElMessage } from 'element-plus'
import { planStandardPartListPM as permission } from '@/page-permission/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const originRow = ref({})
const currentMonomer = ref({})
const currentArea = ref({})
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
  wrapperBox: '.plan-standard-part',
  paginate: true,
  extraHeight: 40
})

function currentChange(val) {
  currentMonomer.value = val
}

function currentAreaChange(val) {
  currentArea.value = val
}

provide('currentMonomer', currentMonomer)
provide('globalProject', globalProject)
provide('currentArea', currentArea)

const tableRules = {
  name: [{ required: true, message: '请输入名称', trigger: 'blur' }],
  useProperty: [{ required: true, message: '请输入选择使用范围', trigger: 'change' }],
  specification: [{ required: true, message: '请输入规格', trigger: 'blur' }],
  measureUnit: [{ required: true, message: '请输入单位', trigger: 'blur' }],
  quantity: [{ required: true, message: '请输入数量', trigger: 'change' }]
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

function editRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
}
async function deleteRow(row) {
  try {
    await crudApi.del([row.id])
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('删除', e)
  }
}
function rowCancel(row) {
  row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  row.isModify = false
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
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.isModify = false
  } catch (e) {
    console.log(`修改`, e)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content.map(v => {
    v.projectId = v.project.id
    if (isNotBlank(v.monomer)) {
      v.monomerId = v.monomer.id
    }
    if (isNotBlank(v.area)) {
      v.areaId = v.area.id
    }
    return v
  })
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

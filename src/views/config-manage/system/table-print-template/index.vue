<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="表格名称" min-width="140px">
        <template #default="{ row }">
          <table-cell-tag v-if="row.isDefault" name="默认" />
          <span style="margin-left: 15px">{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('moduleType')" key="moduleType" prop="moduleType" :show-overflow-tooltip="true" label="模块" min-width="140px">
        <template #default="{ row }">
          <span>{{ moduleTypeEnum[row.moduleType] ? moduleTypeEnum[row.moduleType].L : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" :show-overflow-tooltip="true" label="表格" min-width="140px">
        <template #default="{ row }">
          <span>{{ tableTypeEnum[row.type] ? tableTypeEnum[row.type].L : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="160px" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" label="状态" align="center" min-width="260">
        <template #default="{ row }">
          <el-switch
            v-model="row.enabled"
            :disabled="!checkPermission(permission.edit)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="systemEnabledEnum.ENUM.TRUE.V"
            :inactive-value="systemEnabledEnum.ENUM.FALSE.V"
            @change="changeEnabled(row, row.enabled)"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="170px"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <ud-operation :show-detail="true" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/config/system-config/table-print-template'
import { ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { systemEnabledEnum } from '@enum-ms/system'
import { ElMessageBox } from 'element-plus'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import { moduleTypeEnum, tableTypeEnum } from '@/utils/print/table-type'

// crud交由presenter持有
const permission = {
  get: ['tablePrinting:get'],
  add: ['tablePrinting:add'],
  edit: ['tablePrinting:edit'],
  del: ['tablePrinting:del']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '表格模板',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

async function changeEnabled(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + systemEnabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, enabled: val })
    crud.refresh()
    crud.notify(systemEnabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更打印模板状态', error)
    data.enabled = data.enabled === systemEnabledEnum.ENUM.TRUE.V ? systemEnabledEnum.ENUM.FALSE.V : systemEnabledEnum.ENUM.TRUE.V
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

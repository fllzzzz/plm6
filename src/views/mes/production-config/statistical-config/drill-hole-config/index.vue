<template>
  <div class="app-container">
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
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="specPrefix" :show-overflow-tooltip="true" label="零件规格前缀" align="center" width="160px" />
      <el-table-column :show-overflow-tooltip="true" label="板厚数值（毫米）" align="center" min-width="200px">
        <template #default="{ row }">
          <span>{{ row.minThickness }}</span>
          <span> ~ </span>
          <span>{{ row.maxThickness }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="孔径数值范围（毫米）" align="center" min-width="200px">
        <template #default="{ row }">
          <span>{{ row.minBoreDiameter }}</span>
          <span> ~ </span>
          <span>{{ row.maxBoreDiameter }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="unitPrice" label="单价（元/个）" align="center" width="100px" />
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="150px" align="center">
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <batch-form />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/drill-hole-config'
import { ref } from 'vue'

import { configStatisticalDrillHolePM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import batchForm from './module/batch-form'
import mForm from './module/form'

const optShow = {
  add: true,
  batchAdd: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud } = useCRUD(
  {
    title: '钻孔配置',
    sort: [],
    formStore: true,
    formStoreKey: 'MES_STATISTICAL_DRILL_HOLE_CONFIG',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    dataPath: ''
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>

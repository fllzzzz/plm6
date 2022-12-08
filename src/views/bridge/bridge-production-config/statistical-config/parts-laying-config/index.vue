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
      :data-format="dataFormat"
      style="width: 100%"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="specPrefix" :show-overflow-tooltip="true" label="截面类型" align="center" min-width="160px" />
      <el-table-column prop="name" :show-overflow-tooltip="true" label="下料方式" align="center" min-width="160px">
        <template #default="{ row }">
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="板厚范围" align="center" min-width="200px">
        <template #default="{ row }">
          <span>{{ row.minNumerical }}</span>
          <span> ~ </span>
          <span>{{ row.maxNumerical }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="wageQuotaType" :show-overflow-tooltip="true" label="计量方式" align="center" min-width="160px" />
      <el-table-column :show-overflow-tooltip="true" prop="unitPrice" label="单价" align="center" min-width="100px">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.unitPrice }} </span>
          <span>{{ wageQuotaTypeEnum.V[row.wageQuotaType].unit }}</span>
        </template>
      </el-table-column>
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
import crudApi from '@/api/bridge/production-config/parts-laying-config'
import { ref } from 'vue'

import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { bridgeConfigStatisticalPartsLayingPM as permission } from '@/page-permission/config'
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

const dataFormat = [['wageQuotaType', ['parse-enum', wageQuotaTypeEnum]]]

const tableRef = ref()
const { crud } = useCRUD(
  {
    title: '零件-下料',
    sort: [],
    formStore: true,
    formStoreKey: 'MES_STATISTICAL_PARTS_LAYING_CONFIG',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    dataPath: ''
  },
  tableRef
)

const { maxHeight } = useMaxHeight()
</script>

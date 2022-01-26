<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader print-key="mesStructureProductionReport">
        <template v-slot:summaryText="{ summary }">
          <span>{{ summary?.quantity }}件</span> /
          <span>{{ toFixed(summary?.totalNetWeight, DP.COM_WT__KG) }}kg</span>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="rowId"
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showProject showMonomer />
      <productType-base-info-columns :productType="productType" :columns="columns" />
      <productType-spec-info-columns :productType="productType" :columns="columns">
        <template #quantity>
          <el-table-column
            v-if="columns.visible('quantity')"
            prop="quantity"
            sortable="custom"
            label="数量"
            align="center"
            min-width="70px"
          />
        </template>
      </productType-spec-info-columns>
      <el-table-column
        v-if="columns.visible('date')"
        key="date"
        prop="date"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="生产日期"
        align="center"
        width="160px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.date }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi, { getSummary } from '@/api/mes/production-manage/report/artifact'
import { ref, provide } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { artifactProductionReportPM as permission } from '@/page-permission/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import productTypeSpecInfoColumns from '@comp-mes/table-columns/productType-spec-info-columns'
import pagination from '@crud/Pagination'
import mHeader from '../components/report-header.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productType = componentTypeEnum.ARTIFACT.V
provide('getSummaryApi', getSummary)
provide('productType', productType)
provide('defaultQuery', {})

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产报表-结构报表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['totalNetWeight', 'totalGrossWeight', 'drawingNumber', 'surfaceArea', 'remark']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    return v
  })
}
</script>

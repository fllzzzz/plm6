<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader print-key="mesEnclosureProductionReport">
        <template #customSearch="{ query }">
          <common-radio-button
            v-model="query.category"
            :options="mesEnclosureTypeEnum.ENUM"
            showOptionAll
            type="enum"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template v-slot:summaryText="{ summary }">
          <span>{{ summary?.quantity }}张</span> /
          <span>{{ toFixed(summary?.totalLength, DP.MES_ENCLOSURE_L__M) }}m</span>
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
      <productType-base-info-columns :productType="productType" :category="crud.query.category" :columns="columns" />
      <productType-spec-info-columns :productType="productType" :category="crud.query.category" :columns="columns">
        <template #quantity>
          <el-table-column
            v-if="columns.visible('quantity')"
            key="quantity"
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
import crudApi, { getSummary } from '@/api/mes/production-manage/report/enclosure'
import { ref, provide } from 'vue'

import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { enclosureProductionReportPM as permission } from '@/page-permission/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'

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

const productType = componentTypeEnum.ENCLOSURE.V
provide('getSummaryApi', getSummary)
provide('productType', productType)
provide('defaultQuery', {})

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产报表-围护',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['remark', 'weight']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.totalArea = convertUnits(v.totalArea, 'mm²', '㎡', DP.COM_AREA__M2)
    v.totalLength = convertUnits(v.totalLength, 'mm', 'm', DP.COM_L__M)
    return v
  })
}
</script>

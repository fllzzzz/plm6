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
      row-key="rowId"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showWorkshop showProductionLine showProcess showTeam />
      <el-table-column
        v-if="columns.visible('taskMete')"
        prop="taskMete"
        align="center"
        :show-overflow-tooltip="true"
        :label="`任务量(${unitObj.unit})`"
        min-width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeMete')"
        prop="completeMete"
        align="center"
        :show-overflow-tooltip="true"
        :label="`生产量(${unitObj.unit})`"
        min-width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.completeMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('price')"
        key="price"
        prop="price"
        align="center"
        :show-overflow-tooltip="true"
        label="工资总额(元)"
        min-width="100px"
      >
        <template #default="{ row }">
          <span v-to-fixed="{ k: 'YUAN', val: row.price }"></span>
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.detail]" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/off-staff-settlement'
import { ref, provide, computed } from 'vue'

import { offStaffSettlementPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '编外-工资结算',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

provide('query', crud.query)

const productType = computed(() => {
  return crud.query.productType
})

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: productType.value
  })
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.taskMete = useProductMeteConvert({
      productType: productType.value,
      weight: { num: v.taskNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp },
      length: { num: v.taskLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.completeMete = useProductMeteConvert({
      productType: productType.value,
      weight: { num: v.completeNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp },
      length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    return v
  })
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}
</script>

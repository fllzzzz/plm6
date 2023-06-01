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
      show-summary
      row-key="rowId"
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showWorkshop showProductionLine showProcess showTeam />
      <!-- <el-table-column
        v-if="columns.visible('wageQuotaType')"
        key="wageQuotaType"
        prop="wageQuotaType"
        :show-overflow-tooltip="true"
        label="单位"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ wageQuotaTypeEnum.V[scope.row.wageQuotaType].meteUnit }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('productMete')"
        prop="productMete"
        align="center"
        :show-overflow-tooltip="true"
        :label="`生产量(${unitObj.unit})`"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productMete }}</span>
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
        <template v-slot="scope">
          <span v-to-fixed="{ k: 'YUAN', val: scope.row.price }"></span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.detail]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/in-staff/piecework-system'
import { ref, provide, computed } from 'vue'

import { DP } from '@/settings/config'

// import { wageQuotaTypeEnum } from '@enum-ms/mes'
// import { inStaffPieceworkSystemPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const permission = {
  get: []
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '编内-计件制',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

provide('query', crud.query)

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: crud.query.productType
  })
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    // v.productMete = useWageQuotaMeteConvert({
    //   length: v.mate,
    //   weight: v.mate,
    //   surfaceArea: v.mate,
    //   wageQuotaType: v.wageQuotaType
    // }).convertMete
    v.productMete = v.mate
    return v
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'price') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index].toFixed(DP.YUAN)
      }
    }
  })
  return sums
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}
</script>

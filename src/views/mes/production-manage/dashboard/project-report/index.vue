<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <component :is="currentView" :columns="columns" />
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        :label="`数量(${unitObj.measure})`"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('mete')"
        prop="mete"
        :show-overflow-tooltip="true"
        :label="`${unitObj.label}(${unitObj.unit})`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.mete }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeQuantity')"
        prop="completeQuantity"
        sortable="custom"
        :label="`已生产数(${unitObj.measure})`"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('completeMete')"
        prop="completeMete"
        sortable="custom"
        :label="`已生产量(${unitObj.unit})`"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('sendQuantity')"
        prop="sendQuantity"
        sortable="custom"
        :label="`已发运数(${unitObj.measure})`"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('shipMete')"
        prop="shipMete"
        sortable="custom"
        :label="`已发运量(${unitObj.unit})`"
        align="center"
        min-width="70px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { artifact, enclosure } from '@/api/mes/production-manage/dashboard/project-report'
import { ref, computed, provide } from 'vue'

import { projectComponentTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { projectReportDashboardPM as permission } from '@/page-permission/mes'

import artifactComponent from './artifact'
import enclosureComponent from './enclosure'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '项目报表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: artifact }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const currentView = computed(() => {
  if (crud.query.category === projectComponentTypeEnum.ARTIFACT.V) {
    return artifactComponent
  } else {
    return enclosureComponent
  }
})

const productType = computed(() => {
  if (crud.query.category === projectComponentTypeEnum.ARTIFACT.V) {
    return componentTypeEnum.ARTIFACT.V
  } else {
    return componentTypeEnum.ENCLOSURE.V
  }
})

CRUD.HOOK.beforeToQuery = () => {
  crud.crudApi.get = crud.query.category === projectComponentTypeEnum.ARTIFACT.V ? artifact : enclosure
}

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: productType.value,
    w_unit: 'kg',
    isSingle: true
  })
})
provide('unitObj', unitObj)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.mete = useProductMeteConvert({
      productType: productType.value,
      length: { num: v.length, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.netWeight, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.completeMete = useProductMeteConvert({
      productType: productType.value,
      length: { num: v.length * v.completeQuantity, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.netWeight * v.completeQuantity, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.shipMete = useProductMeteConvert({
      productType: productType.value,
      length: { num: v.length * v.sendQuantity, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.netWeight * v.sendQuantity, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.sendQuantity = v.sendQuantity || 0
    return v
  })
}
</script>

<template>
  <div class="app-container">
    <div class="head-container">
      <slot name="header"></slot>
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
      <belonging-info-columns :columns="columns" showFactory showWorkshop showProductionLine />
      <el-table-column
        v-if="columns.visible('processName')"
        prop="processName"
        :show-overflow-tooltip="true"
        label="工序"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <el-tag effect="plain">{{ row.processName }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('organizationType')"
        prop="organizationType"
        :show-overflow-tooltip="true"
        label="属性"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <el-tag effect="plain" :type="teamAttributeEnum.V[row.organizationType].T">{{
            teamAttributeEnum.VL[row.organizationType]
          }}</el-tag>
        </template>
      </el-table-column>
      <belonging-info-columns :columns="columns" showTeam :teamAlign="'center'" />
      <el-table-column prop="showUnit" :show-overflow-tooltip="true" label="单位" align="center" width="100">
        <template #default="{ row }">
          <span>{{ row.showUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" :show-overflow-tooltip="true" label="生产量" align="center">
        <template #default="{ row }">
          <span>{{ row.mete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="price" :show-overflow-tooltip="true" label="工资总额(元)" align="center">
        <template #default="{ row }">
          <span v-to-fixed="{ k: 'YUAN', val: row.price }"></span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.detail]" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail v-model:visible="detailVisible" :info="itemInfo" :title="`${name}班组工资详情`" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/in-staff/piecework-system'
import { ref, provide, inject } from 'vue'

import { teamAttributeEnum } from '@enum-ms/mes'
import { inStaffPieceworkSystemPM as permission } from '@/page-permission/mes'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import pagination from '@crud/Pagination'
import mDetail from './detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '班组工资',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

provide('query', crud.query)
const name = inject('name')

const processList = ref([])

CRUD.HOOK.handleRefresh = (crud, res) => {
  processList.value = res.data.content.length && res.data.content[0]?.processPrice
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    const { c_unit, unit, dp } = useProductSummaryMeteUnit({ productType: v.productType })
    v.showUnit = c_unit
    v.mete = useProductMeteConvert({
      productType: v.productType,
      weight: { num: v.mate, to: unit, dp },
      length: { num: v.mate, to: unit, dp }
    })
    return v
  })
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  itemInfo.value = deepClone(row)
  detailVisible.value = true
}
</script>

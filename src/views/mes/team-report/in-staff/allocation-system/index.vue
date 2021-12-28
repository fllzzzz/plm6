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
      row-key="id"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showWorkshop showProductionLine />
      <el-table-column
        v-if="columns.visible('completeQuantity')"
        key="completeQuantity"
        prop="completeQuantity"
        :show-overflow-tooltip="true"
        label="生产数量"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeMete')"
        key="completeMete"
        prop="completeMete"
        :show-overflow-tooltip="true"
        :label="`生产量(${unitObj.unit})`"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('price')"
        key="price"
        prop="price"
        :show-overflow-tooltip="true"
        label="工资总额(元)"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.price }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="【工序 │ 总额】"
        min-width="260px"
      >
        <template v-slot="scope">
          <span v-html="scope.row.processSequence" />
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.get]" label="操作" width="100px" align="center">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo"></mDetail>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/in-staff/allocation-system'
import { ref, provide, computed } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '编内-分配制',
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
  return useProductSummaryMeteUnit({ productType: crud.query.productType })
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.processSequence = v.processList.map((o) => {
      return `<span>【${o.processName} │ <span style="color: #67C23A;">${o.price}</span>】</span>`
    })
    const complete = useProductMeteConvert({
      productType: crud.query.productType,
      length: v.completeLength,
      L_TO_UNIT: unitObj.value.unit,
      L_DP: unitObj.value.dp,
      weight: v.completeNetWeight,
      W_TO_UNIT: unitObj.value.unit,
      W_DP: unitObj.value.dp
    })
    v.completeMete = complete.convertMete
    return v
  })
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  // itemInfo.value = row
  // detailVisible.value = true
}
</script>

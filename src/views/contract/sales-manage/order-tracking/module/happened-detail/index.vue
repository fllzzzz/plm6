<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('monomer.name')" key="monomer.name" prop="monomer.name" label="单体" min-width="140" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.monomer?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('area.name')" prop="area.name" key="area.name" label="区域" min-width="140" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('name')" prop="name" key="name" label="名称" align="center" min-width="110" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" key="serialNumber" label="编号" align="center" min-width="110" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('specification')" prop="specification" key="specification" label="规格" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('material')" prop="material" key="material" label="材质" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('measure')" prop="measure" key="measure" label="计量单位" align="center" width="70" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('quantity')" prop="quantity" key="quantity" label="数量" align="center" min-width="60" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('nuclear')" prop="nuclear" key="nuclear" label="核算单位" align="center" width="70" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('totalMete')" prop="totalMete" key="totalMete" label="总量" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('unitPrice')" prop="unitPrice" key="unitPrice" label="单价" align="right" min-width="80" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" key="totalPrice" label="总价" align="right" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('auditTime')" prop="auditTime" key="auditTime" label="发运时间"  align="center"  width="130" show-overflow-tooltip />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { shipRecord as get } from '@/api/contract/sales-manage/order-tracking'
import { ref, inject, watch, nextTick, computed } from 'vue'

import { orderTrackingPM as permission } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const projectId = inject('projectId')

const { decimalPrecision } = useDecimalPrecision()

watch(
  projectId,
  (id) => {
    nextTick(() => {
      crud.query.projectId = id
      crud.refresh()
    })
  },
  { immediate: true }
)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dataFormat = computed(() => {
  return [
    ['auditTime', 'parse-time'],
    ['unitPrice', ['to-thousand', decimalPrecision.value.contract]],
    ['totalPrice', ['to-thousand', decimalPrecision.value.contract]]
  ]
})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '发运记录',
    sort: [],
    permission: { get: permission.detail },
    crudApi: { get },
    requiredQuery: ['projectId'],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    if (row.totalMete) {
      row.totalMete = Number(row.totalMete.toFixed(2)) ? row.totalMete.toFixed(2) : Number(row.totalMete.toFixed(5))
    }
  })
}
</script>

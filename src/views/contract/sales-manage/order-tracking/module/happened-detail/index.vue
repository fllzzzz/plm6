<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('monomer.name')" key="monomer.name" prop="monomer.name" label="单体" min-width="140" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.monomer.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('area.name')" prop="area.name" key="area.name" label="区域" min-width="140" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.area.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('name')" prop="name" key="name" label="名称" align="center" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" key="serialNumber" label="编号" align="center" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specification')" prop="specification" key="specification" label="规格" align="center" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('material')" prop="material" key="material" label="材质" align="center" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('measureUnit')" prop="measureUnit" key="measureUnit" label="计量单位" align="center" min-width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.measureUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('quantity')" prop="quantity" key="quantity" label="数量" align="center" min-width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('accountingUnit')" prop="accountingUnit" key="accountingUnit" label="核算单位" align="center" min-width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalMete')" prop="totalMete" key="totalMete" label="总量" align="center" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.totalMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('price')" prop="price" key="price" label="单价" align="right" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-thousand="row.price" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" key="totalPrice" label="总价" align="right" min-width="110" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-thousand="row.totalPrice" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('happenedTime')" prop="happenedTime" key="happenedTime" label="发运时间"  align="center"  width="130" show-overflow-tooltip >
        <template #default="{ row }">
          <span v-parse-time="row.happenedTime" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { shipRecord as get } from '@/api/contract/sales-manage/order-tracking'
import { ref, inject, watch, nextTick } from 'vue'

import { orderTrackingPM as permission } from '@/page-permission/contract'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const projectId = inject('projectId')

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
const { crud, columns } = useCRUD(
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
</script>
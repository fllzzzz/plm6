<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" class="enclosure-container" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('area.name')"
        key="area.name"
        prop="area.name"
        show-overflow-tooltip
        label="批次"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        show-overflow-tooltip
        label="名称"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        show-overflow-tooltip
        label="编号"
        align="center"
        min-width="130"
      />
      <el-table-column
        v-if="columns.visible('plate')"
        key="plate"
        prop="plate"
        show-overflow-tooltip
        label="板型"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        show-overflow-tooltip
        label="单长(mm)"
        align="right"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('surfaceArea')"
        key="surfaceArea"
        prop="surfaceArea"
        show-overflow-tooltip
        label="单面积(mm²)"
        align="right"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('totalQuantity')"
        key="totalQuantity"
        prop="totalQuantity"
        label="数量(张)"
        align="center"
        min-width="70"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        show-overflow-tooltip
        label="总长度(m)"
        align="right"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        show-overflow-tooltip
        label="总面积(m²)"
        align="right"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('pricingManner')"
        key="pricingManner"
        prop="pricingManner"
        align="center"
        min-width="120"
        label="计价方式"
      />
      <el-table-column
        v-if="columns.visible('unitPrice')"
        key="unitPrice"
        prop="unitPrice"
        align="right"
        min-width="120"
        label="综合单价"
      />
      <el-table-column
        v-if="columns.visible('totalPrice')"
        key="totalPrice"
        prop="totalPrice"
        align="right"
        min-width="120"
        label="金额"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        align="center"
        label="发运日期"
        width="100"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { enclosureList as get } from '@/api/contract/sales-manage/shipment-tracking'
import { ref, defineEmits } from 'vue'

import { shipmentTrackingPM as permission } from '@/page-permission/contract'
import { enclosurePricingMannerEnum } from '@enum-ms/enclosure'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const emit = defineEmits(['resetQuery'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['totalLength', ['to-thousand', DP.MES_ENCLOSURE_L__M]],
  ['totalArea', ['to-thousand', DP.MES_ENCLOSURE_L__M]],
  ['unitPrice', 'to-thousand'],
  ['totalPrice', 'to-thousand'],
  ['pricingManner', ['parse-enum', enclosurePricingMannerEnum]]
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护制品',
    sort: [],
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    invisibleColumns: [],
    requiredQuery: ['projectId']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.common-container', '.enclosure-container'],
  minHeight: 300,
  paginate: true
})

// 重置前
CRUD.HOOK.beforeResetQuery = () => {
  emit('resetQuery')
}
</script>

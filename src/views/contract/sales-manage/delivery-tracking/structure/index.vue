<template>
  <div>
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
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="单体"  min-width="80" />
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="区域"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="名称"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="编号"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="规格" min-width="80"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="材质"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="数量"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="总重(kg)"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="单价(元)"/>
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="发运日期"  min-width="80" />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/order-tracking'
import { ref, computed } from 'vue'

import { projectStatusEnum, orderSourceTypeEnum } from '@enum-ms/contract'
import { orderTrackingPM as permission } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '构件-发运跟踪',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    requiredQuery: ['orderId'],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true, extraHeight: 100 })

const dataFormat = computed(() => {
  return [
    ['project', 'parse-project'],
    ['orderSourceType', ['parse-enum', orderSourceTypeEnum]],
    ['status', ['parse-enum', projectStatusEnum]],
    ['contractAmount', ['to-thousand-ck', decimalPrecision.value.contract]]
  ]
})

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  // data.content.forEach(v => {
  //   // 收款比例
  //   v.collectionRate = (v.settlementAmount || v.contractAmount) ? (v.collectionAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
  //   // 开票比例
  //   v.invoiceRate = (v.settlementAmount || v.contractAmount) ? (v.invoiceAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
  //   // 出库比例
  //   v.happenedRate = (v.settlementAmount || v.contractAmount) ? (v.happenedAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
  //   // 入库比例
  //   v.warehouseRate = (v.settlementAmount || v.contractAmount) ? (v.warehouseAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
  // })
}

</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
  color:#409eff;
}
</style>

<template>
  <div class="app-container wrap">
    <!--工具栏-->
    <div class="wrap-left">
      <mHeader ref="headerRef" />
      <!-- 订单列表 -->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data-format="dataFormat"
        :data="crud.data"
        style="width: 100%"
        highlight-current-row
        row-key="id"
        @row-click="handleRowChange"
        :max-height="maxHeight"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购合同编号" align="center" min-width="130" />
        <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="right" min-width="120" show-overflow-tooltip />
        <el-table-column v-if="columns.visible('inboundAmount')" prop="inboundAmount" key="inboundAmount" label="入库额" align="right" min-width="120" show-overflow-tooltip>
          <template #default="{ row }">
            <div>{{ row.inboundAmount }}</div>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
    <div class="wrap-right">
      <inboundRecord :detail-info="detailInfo" style="width:100%;" :permission="permission"/>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, provide } from 'vue'

import { supplierOrderLedgerPM as permission } from '@/page-permission/supply-chain'
// import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import inboundRecord from './module/inbound-record'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
// const applicationVisible = ref(false)
// const settleVisible = ref(false)

const dataFormat = ref([
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand']
])

const { crud, columns } = useCRUD(
  {
    title: '订单查询',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const orderId = ref(undefined)

provide('orderId', orderId)

function handleRowChange(row) {
  detailInfo.value = row
  orderId.value = row.id
}

</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
}
.wrap{
  display:flex;
  .wrap-left{
    width:30%;
  }
  .wrap-right{
    width:70%;
    padding-left:20px;
  }
}
</style>

<template>
  <div class="app-container">
    <!--工具栏-->
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
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('orderSerialNumber')" key="orderSerialNumber" prop="orderSerialNumber" :show-overflow-tooltip="true" label="订单号" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.orderSerialNumber }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="供应商" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.supplierName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('propertyType')" key="propertyType" prop="propertyType" label="属性" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.propertyType? supplierPayMentTypeEnum.VL[scope.row.propertyType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('basicClassName')" key="basicClassName" prop="basicClassName" :show-overflow-tooltip="true" label="种类" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.basicClassName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentReason')" key="paymentReason" :show-overflow-tooltip="true" prop="paymentReason" label="付款事由" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.paymentReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.paymentReason]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('payForType')" key="payForType" :show-overflow-tooltip="true" prop="payForType" label="费用类别" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.payForType? contractPayForEnum.VL[scope.row.payForType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentUnit')" key="paymentUnit" :show-overflow-tooltip="true" prop="paymentUnit" label="付款单位" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.paymentUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveUnit')" key="receiveUnit" prop="receiveUnit" :show-overflow-tooltip="true" label="收款单位" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.receiveUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentDate')" key="paymentDate" :show-overflow-tooltip="true" prop="paymentDate" label="付款日期" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" :show-overflow-tooltip="true" prop="paymentAmount" label="付款金额(元)" align="center" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.paymentAmount && scope.row.paymentAmount>0? toThousand(scope.row.paymentAmount): scope.row.paymentAmount }}</span>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-list'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { supplierPayMentTypeEnum, contractPayForEnum } from '@enum-ms/contract'
import useDict from '@compos/store/use-dict'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'

// crud交由presenter持有
const permission = {
  get: ['supplierPayList:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dict = useDict(['payment_reason'])
const { crud, columns } = useCRUD(
  {
    title: '付款台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplierPayList',
  paginate: true,
  extraHeight: 40
})

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>

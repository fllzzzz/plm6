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
      <template v-if="crud.query.type == supplierPayTypeEnum.TRANSPORT.V">
        <el-table-column
          v-if="columns.visible('purchaseSn')"
          key="purchaseSn"
          prop="purchaseSn"
          :show-overflow-tooltip="true"
          label="所属订单"
          min-width="120"
        >
          <template v-slot="scope">
            <div>{{ scope.row.purchaseSn }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-else>
        <el-table-column
          v-if="columns.visible('orderSerialNumber')"
          key="orderSerialNumber"
          prop="orderSerialNumber"
          :show-overflow-tooltip="true"
          label="订单号"
          min-width="120"
        >
          <template v-slot="scope">
            <div>{{ scope.row.orderSerialNumber }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type != supplierPayTypeEnum.TRANSPORT.V">
        <el-table-column
          v-if="columns.visible('signingTime')"
          key="signingTime"
          prop="signingTime"
          :show-overflow-tooltip="true"
          label="签订日期"
          min-width="120"
        >
          <template v-slot="scope">
            <div>{{ scope.row.signingTime? parseTime(scope.row.signingTime,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-else>
        <el-table-column
          v-if="columns.visible('orderSerialNumber')"
          key="orderSerialNumber"
          prop="orderSerialNumber"
          :show-overflow-tooltip="true"
          label="物流单位"
          min-width="120"
        >
          <template v-slot="scope">
            <div>{{ scope.row.orderSerialNumber }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type == supplierPayTypeEnum.MATERIAL.V || crud.query.type == supplierPayTypeEnum.PRODUCT.V">
        <el-table-column
          v-if="columns.visible('supplierName')"
          key="supplierName"
          prop="supplierName"
          :show-overflow-tooltip="true"
          label="供应商"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <div>{{ scope.row.supplierName }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type == supplierPayTypeEnum.TRANSPORT.V">
        <el-table-column
          v-if="columns.visible('propertyType')"
          key="propertyType"
          prop="propertyType"
          label="属性"
          align="center"
          min-width="80"
        >
          <template v-slot="scope">
            <div>{{ scope.row.propertyType ? supplierPayMentTypeEnum.VL[scope.row.propertyType] : '' }}</div>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type == supplierPayTypeEnum.SUBCONTRACT.V">
        <el-table-column
          v-if="columns.visible('subcontract')"
          key="subcontract"
          prop="subcontract"
          label="分包单位"
          align="center"
          min-width="80"
        >
          <template v-slot="scope">
            <div>{{ scope.row.subcontract }}</div>
          </template>
        </el-table-column>
      </template>
      <el-table-column
        v-if="columns.visible('basicClassName')"
        key="basicClassName"
        prop="basicClassName"
        :show-overflow-tooltip="true"
        label="种类"
        align="center"
        min-width="80"
      >
        <template v-slot="scope">
          <div>{{ scope.row.basicClassName }}</div>
        </template>
      </el-table-column>
      <template v-if="crud.query.type != supplierPayTypeEnum.TRANSPORT.V">
        <el-table-column
          v-if="columns.visible('amount')"
          key="amount"
          :show-overflow-tooltip="true"
          prop="amount"
          label="合同金额(元)"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <span>{{ scope.row.amount && scope.row.amount > 0 ? toThousand(scope.row.amount) : scope.row.amount }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-else>
        <el-table-column
          v-if="columns.visible('freight')"
          key="freight"
          :show-overflow-tooltip="true"
          prop="freight"
          label="运输费(元)"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <span>{{ scope.row.freight && scope.row.freight > 0 ? toThousand(scope.row.freight) : scope.row.freight }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type != supplierPayTypeEnum.TRANSPORT.V">
        <el-table-column
          v-if="columns.visible('settlementAmount')"
          key="settlementAmount"
          :show-overflow-tooltip="true"
          prop="settlementAmount"
          label="结算金额(元)"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <span>{{
              scope.row.settlementAmount && scope.row.settlementAmount > 0
                ? toThousand(scope.row.settlementAmount)
                : scope.row.settlementAmount
            }}</span>
          </template>
        </el-table-column>
      </template>
      <el-table-column
        v-if="columns.visible('paymentAmount')"
        key="paymentAmount"
        :show-overflow-tooltip="true"
        prop="paymentAmount"
        label="付款金额(元)"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{
            scope.row.paymentAmount && scope.row.paymentAmount > 0 ? toThousand(scope.row.paymentAmount) : scope.row.paymentAmount
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('payRate')"
        key="payRate"
        :show-overflow-tooltip="true"
        prop="payRate"
        label="付款比例"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{ scope.row.payRate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('invoiceAmount')"
        key="invoiceAmount"
        :show-overflow-tooltip="true"
        prop="invoiceAmount"
        label="收票金额(元)"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{
            scope.row.invoiceAmount && scope.row.invoiceAmount > 0 ? toThousand(scope.row.invoiceAmount) : scope.row.invoiceAmount
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('invoiceRate')"
        key="invoiceRate"
        :show-overflow-tooltip="true"
        prop="invoiceRate"
        label="收票比例"
        align="center"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{ scope.row.invoiceRate }}</span>
        </template>
      </el-table-column>
      <template v-if="crud.query.type == supplierPayTypeEnum.MATERIAL.V || crud.query.type == supplierPayTypeEnum.PRODUCT.V">
        <el-table-column
          v-if="columns.visible('inboundAmount')"
          key="inboundAmount"
          :show-overflow-tooltip="true"
          prop="inboundAmount"
          label="入库金额(元)"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <span>{{
              scope.row.inboundAmount && scope.row.inboundAmount > 0 ? toThousand(scope.row.inboundAmount) : scope.row.inboundAmount
            }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-if="crud.query.type == supplierPayTypeEnum.SUBCONTRACT.V || crud.query.type == supplierPayTypeEnum.PRODUCT.V">
        <el-table-column
          v-if="columns.visible('projectList')"
          key="projectList"
          prop="projectList"
          label="关联项目"
          align="center"
          min-width="80"
        >
          <template v-slot="scope">
            <template v-if="scope.row.projectList && scope.row.projectList.length > 0">
              <div v-for="item in scope.row.projectList" :key="item.id">{{ item.serialNumber + ' ' + item.shortName }}</div>
            </template>
          </template>
        </el-table-column>
      </template>
      <!-- 操作按钮 -->
      <!-- <el-table-column
        v-if="columns.visible('paymentAmount')"
        key="paymentAmount"
        :show-overflow-tooltip="true"
        prop="paymentAmount"
        label="订单信息"
        align="center"
        min-width="100"
      >
        <template v-slot="scope"> -->
          <!-- <span>{{
            scope.row.paymentAmount && scope.row.paymentAmount > 0 ? toThousand(scope.row.paymentAmount) : scope.row.paymentAmount
          }}</span> -->
        <!-- </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/payboard'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import {
  supplierPayMentTypeEnum,
  supplierPayTypeEnum
} from '@enum-ms/contract'

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
  td:nth-child(1) {
    .cell {
      opacity: 0;
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

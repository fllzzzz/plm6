<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.dateType"
        :options="contractReceiveDateTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="日期类型"
        style="width:110px"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:200px"
      />
      <el-input
        v-model="query.orderSerialNumber"
        placeholder="订单号"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model="query.supplierName"
        placeholder="供应商"
        style="width:200px"
        class="filter-item"
      />
      <common-select
        v-model="query.propertyType"
        :options="supplierPayMentTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="属性"
        style="width:200px"
        @change="crud.toQuery"
      />
      <basic-class-select
        v-model="query.basicClass"
        clearable
        placeholder="物料种类"
        style="width:200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.invoiceType"
        :options="invoiceTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="发票类型"
        style="width:200px"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.auditStatus"
        :options="auditTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="状态"
        style="width:200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.receiveInvoiceUnit"
        placeholder="收票单位"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model="query.invoiceUnit"
        placeholder="开票单位"
        style="width:200px"
        class="filter-item"
      />

      <el-input
        v-model="query.writtenByName"
        placeholder="填报人"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model="query.auditUserName"
        placeholder="审核人"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
      <crudOperation add-text="收票填报">
        <template #viewLeft>
          <el-tag type="success" v-if="totalSum" size="medium" style="margin-right:5px;">{{ `累计收票金额：${totalSum.toThousand()}元` }}</el-tag>
          <el-tag type="success" v-if="totalTax" size="medium">{{ `累计进项税额：${totalTax.toThousand()}元` }}</el-tag>
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { supplierPayMentTypeEnum, auditTypeEnum, contractReceiveDateTypeEnum, invoiceTypeEnum } from '@enum-ms/contract'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import { receiveSum } from '@/api/contract/supplier-manage/pay-invoice/invoice'

const defaultQuery = {
  dateType: contractReceiveDateTypeEnum .ENUM.UPDATE_DATE.V,
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  writtenByName: undefined,
  auditStatus: undefined,
  auditUserName: undefined,
  basicClass: undefined,
  orderSerialNumber: undefined,
  invoiceUnit: undefined,
  propertyType: undefined,
  receiveInvoiceUnit: undefined,
  supplierName: undefined
}

const { CRUD, crud, query } = regHeader(defaultQuery)
const totalSum = ref()
const totalTax = ref()

getReceiveSum()

async function getReceiveSum() {
  let data = {}
  try {
    data = await receiveSum()
  } catch (e) {
    console.log('累计收票', e)
  } finally {
    totalSum.value = data.sumInvoiceAmount
    totalTax.value = data.sumTax
  }
}

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime && crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  }
}
</script>

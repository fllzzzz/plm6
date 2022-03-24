<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
      />
      <el-input
        v-model="query.paymentUnit"
        placeholder="付款单位"
        style="width:120px"
        class="filter-item"
      />
      <el-input
        v-model="query.receivingUnit"
        placeholder="收款单位"
        style="width:120px"
        class="filter-item"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="订单号"
        style="width:120px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { auditTypeEnum } from '@enum-ms/contract'
import { paymentSum } from '@/api/contract/supplier-manage/payment-ledger/payment'

const defaultQuery = {
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  paymentUnit: undefined,
  receivingUnit: undefined,
  projectId: undefined,
  serialNumber: undefined,
  auditStatus: { value: auditTypeEnum.PASS.V, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

const totalSum = ref(0)
getSum()

async function getSum() {
  try {
    const data = await paymentSum()
    totalSum.value = data || 0
  } catch (e) {
    console.log('获取累计开票金额', e)
  }
}
</script>

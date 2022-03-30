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
        placeholder="开票单位"
        style="width:120px"
        class="filter-item"
      />
      <el-input
        v-model="query.receivingUnit"
        placeholder="收票单位"
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
      <crudOperation>
        <template #optLeft>
          <el-tag v-if="totalSum" size="medium" class="filter-item">{{ `累计开票:${totalSum?toThousand(totalSum):'-'}元` }}</el-tag>
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { auditTypeEnum } from '@enum-ms/contract'
import { invoiceSum } from '@/api/contract/supplier-manage/payment-ledger/pay-invoice'
import { toThousand } from '@data-type/number'
import crudOperation from '@crud/CRUD.operation'

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
    const data = await invoiceSum()
    totalSum.value = data || 0
  } catch (e) {
    console.log('获取累计收票金额', e)
  }
}
</script>

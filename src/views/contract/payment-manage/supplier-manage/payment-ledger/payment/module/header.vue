<template>
  <div>
    <!-- < size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" /> -->
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <el-date-picker
            v-model="query.createTime"
            type="daterange"
            range-separator=":"
            size="small"
            class="date-item filter-item"
            value-format="x"
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 240px"
            @change="crud.toQuery"
          />
          <el-input v-model.trim="query.paymentUnit" placeholder="付款单位" style="width: 120px" class="filter-item" />
          <el-input v-model.trim="query.receivingUnit" placeholder="收款单位" style="width: 120px" class="filter-item" />
          <!-- <el-input
            v-model.trim="query.serialNumber"
            placeholder="订单号"
            style="width:120px"
            class="filter-item"
          /> -->
          <rrOperation />
        </div>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission?.print"
          api-key="supplierPaymentLedger"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <el-tag type="warning" size="medium" effect="plain">{{`累计付款：${toThousand(totalSum)} 元`}}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { paymentSum } from '@/api/contract/supplier-manage/payment-ledger/payment'
import { ref, watch } from 'vue'

import moment from 'moment'
import { toThousand } from '@data-type/number'
import { auditTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  createTime: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf(),
  paymentUnit: undefined,
  receivingUnit: undefined,
  projectId: undefined,
  serialNumber: undefined,
  auditStatus: { value: auditTypeEnum.PASS.V, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

const totalSum = ref(0)

watch(
  () => query,
  (val) => {
    if (val) {
      getSum()
    } else {
      totalSum.value = 0
    }
  },
  { deep: true, immediate: true }
)

getSum()

async function getSum() {
  try {
    const data = await paymentSum({ startDate: query.startDate, endDate: query.endDate })
    totalSum.value = data || 0
  } catch (e) {
    console.log('获取累计付款金额', e)
  }
}
</script>

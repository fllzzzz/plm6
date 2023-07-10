<template>
  <div>
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
          <el-input v-model.trim="query.supplierName" placeholder="销售单位" style="width: 120px" class="filter-item" />
          <el-input v-model.trim="query.branchCompanyName" placeholder="购方单位" style="width: 120px" class="filter-item" />
          <rrOperation />
        </div>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission?.print"
          api-key="supplierInvoiceLedger"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <el-tag type="warning" effect="plain" size="medium">{{ `累计收票：${toThousand(totalSum,decimalPrecision.contract)}元` }}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { invoiceSum } from '@/api/contract/supplier-manage/payment-ledger/pay-invoice'
import { ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'

import moment from 'moment'
import { auditTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const { decimalPrecision } = useDecimalPrecision()

const defaultQuery = {
  createTime: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf(),
  supplierName: undefined,
  branchCompanyName: undefined,
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
    const data = await invoiceSum({ startDate: query.startDate, endDate: query.endDate })
    totalSum.value = data || 0
  } catch (e) {
    console.log('获取累计收票金额', e)
  }
}
</script>

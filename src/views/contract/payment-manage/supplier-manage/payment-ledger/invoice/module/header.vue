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
        <el-tag v-if="totalSum" type="warning" effect="plain" size="medium">{{ `累计收票：${toThousand(totalSum)}元` }}</el-tag>
      </template>
    </crudOperation>
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
import moment from 'moment'

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

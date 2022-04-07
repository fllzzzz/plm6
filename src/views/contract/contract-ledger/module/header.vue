<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width:100px!important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="编号搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.name"
        placeholder="项目搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.projectManagerName"
        placeholder="业务负责人搜索"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
      <crudOperation>
        <template #optLeft>
          <el-tag v-if="totalSum" size="medium" class="filter-item">{{ `累计合同额:${totalSum.contractAmountSum?toThousand(totalSum.contractAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计结算额:${totalSum.settlementAmountSum?toThousand(totalSum.settlementAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计收款额:${totalSum.collectionAmountSum?toThousand(totalSum.collectionAmountSum):'-'}元` }}</el-tag>
          <el-tag type="success" v-if="totalSum" size="medium" class="filter-item">{{ `累计开票额:${totalSum.invoiceAmountSum?toThousand(totalSum.invoiceAmountSum):'-'}元` }}</el-tag>
        </template>
        <template #viewLeft>
          <print-table
            v-permission="crud.permission.print"
            api-key="contractLedger"
            :params="{ ...query }"
            size="mini"
            type="warning"
            class="filter-item"
          />
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
import { settlementStatusEnum } from '@enum-ms/contract'
import { ledgerSum } from '@/api/contract/contract-ledger'
import { toThousand } from '@data-type/number'

const defaultQuery = {
  projectId: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  serialNumber: undefined,
  name: undefined,
  year: String(new Date().getFullYear()),
  projectManagerName: undefined
}

const { crud, query } = regHeader(defaultQuery)

const totalSum = ref({})

getSum()

async function getSum() {
  try {
    const data = await ledgerSum()
    totalSum.value = data || {}
  } catch (e) {
    console.log('获取累计金额', e)
  }
}
</script>

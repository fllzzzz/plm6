<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.businessTypeEnum"
        :options="businessTypeEnum.ENUM"
        showOptionAll
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <time-range-select :query="query" clearable class="filter-item" style="width: 270px" @change="crud.toQuery" />
      <el-input v-model.trim="query.projectManagerName" placeholder="项目经理搜索" style="width: 200px" class="filter-item" />
      <rrOperation />
      <div v-loading="totalLoading" style="margin-bottom: 6px;">
        <el-row :gutter="10" class="panel-group">
          <el-col :span="6" class="card-panel-col">
            <Panel name="累计合同额：" text-color="#626262" num-color="#1890ff" :end-val="totalSum.contractAmountSum || 0" :precision="2" />
          </el-col>
          <el-col :span="6" class="card-panel-col">
            <Panel name="累计结算额：" text-color="#626262" num-color="#36ae81" :end-val="totalSum.settlementAmountSum || 0" :precision="2" />
          </el-col>
          <el-col :span="6" class="card-panel-col">
            <Panel name="累计收款额：" text-color="#626262" num-color="#36ae81" :end-val="totalSum.collectionAmountSum || 0" :precision="2" />
          </el-col>
          <el-col :span="6" class="card-panel-col">
            <Panel name="累计开票额：" text-color="#626262" num-color="#36ae81" :end-val="totalSum.invoiceAmountSum || 0" :precision="2" />
          </el-col>
        </el-row>
      </div>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table v-permission="crud.permission.print" api-key="contractLedger" :params="{ ...query }" size="mini" type="warning" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { ledgerSum } from '@/api/contract/contract-ledger'

import { settlementStatusEnum } from '@enum-ms/finance'
import { businessTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import Panel from '@/components/Panel'
import timeRangeSelect from '@comp-common/time-range-select/index'

const defaultQuery = {
  projectId: undefined,
  businessTypeEnum: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  dateQueryTypeEnum: undefined,
  startDate: undefined,
  endDate: undefined,
  projectManagerName: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const totalSum = ref({})
const totalLoading = ref(false)

async function getSum() {
  try {
    totalLoading.value = true
    const data = await ledgerSum({ ...crud.query })
    totalSum.value = data || {}
  } catch (e) {
    console.log('获取累计金额', e)
  } finally {
    totalLoading.value = false
  }
}

CRUD.HOOK.handleRefresh = () => {
  getSum()
}
</script>

<style lang="scss" scoped>
.panel-group {
  ::v-deep(.card-panel) {
    .card-panel-description {
      margin: 10px 20px;
      display: flex;
      flex-direction: row;
      justify-content: space-between;
      align-items: flex-start;
      flex-wrap: wrap;
      .card-panel-text {
        margin-top: 2px;
      }
      .card-panel-num {
        font-size: 20px;
      }
    }
  }
}
</style>

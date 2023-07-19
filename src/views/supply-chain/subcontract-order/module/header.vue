<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
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
      <common-radio-button
        v-model="query.boolSettlementStatus"
        :options="subOrderSettleEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="订单号"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.projectName"
        placeholder="项目"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.supplierName"
        placeholder="分包商"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <el-row v-loading="crud.loading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
      <el-col :span="5" class="card-panel-col">
        <Panel name="累计合同额" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.amount || 0" :precision="decimalPrecision.supplyChain" />
      </el-col>
      <el-col :span="5" class="card-panel-col">
        <Panel name="累计订单数" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.orderQuantity || 0" :precision="0" />
      </el-col>
      <el-col :span="5" class="card-panel-col">
        <Panel name="进行中" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.unSettlementQuantity || 0" :precision="0" />
      </el-col>
      <el-col :span="5" class="card-panel-col">
        <Panel name="已结算" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.settlementQuantity || 0" :precision="0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <Panel name="累计结算额" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.settlementAmount || 0" :precision="decimalPrecision.supplyChain" />
      </el-col>
    </el-row>
    <crudOperation add-text="分包立项" />
  </div>
</template>

<script setup>
import { subContractSummary } from '@/api/supply-chain/subcontract-manage/subcontract-order'
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'

import { subOrderSettleEnum } from '@enum-ms/contract'
import checkPermission from '@/utils/system/check-permission'

import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Panel from '@/components/Panel'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const defaultQuery = {
  year: String(new Date().getFullYear()),
  boolSettlementStatus: undefined,
  serialNumber: undefined,
  projectName: undefined,
  supplierName: undefined
}
const { crud, query } = regHeader(defaultQuery)

const totalAmount = ref({})

getSummary()

async function getSummary() {
  try {
    const data = await subContractSummary()
    totalAmount.value = data || {}
  } catch (error) {
    console.log('获取订单数量', error)
  }
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:left;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 18px;
        text-align:right;
      }
    }
  }
}
</style>

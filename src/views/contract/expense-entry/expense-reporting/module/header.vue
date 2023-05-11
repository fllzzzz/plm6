<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <!-- <common-radio-button
        v-model="query.timeType"
        :options="timeTypeEnum.ENUM"
        class="filter-item"
        :showOptionAll="false"
        type="enum"
        @change="handleChange"
      />
      <el-date-picker
        v-if="query.timeType === timeTypeEnum.ALL_YEAR.V"
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-if="query.timeType === timeTypeEnum.CURRENT_MONTH.V"
        v-model="query.month"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择月"
        format="MM"
        value-format="MM"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      /> -->
      <common-select
        v-model="query.expenseTypeId"
        :options="expenseList"
        type="other"
        :data-structure="{ key: 'id', label: 'name', value: 'id' }"
        class="filter-item"
        clearable
        style="width: 200px"
        placeholder="费用类别"
        @change="fetchChange"
      />
      <common-select
        v-model="query.expenseSubjectId"
        :options="subjectList"
        type="other"
        :data-structure="{ key: 'id', label: 'label', value: 'id' }"
        size="small"
        clearable
        class="filter-item"
        placeholder="报销科目"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.reimbursementPerson"
        clearable
        style="width: 150px"
        size="small"
        placeholder="报销人搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.payee"
        clearable
        style="width: 150px"
        size="small"
        placeholder="收款单位搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag effect="plain" class="filter-item" size="medium">
          <span> 报销总额（元）： <span v-thousand="totalAmount" /> </span>
        </el-tag>
        <print-table
          api-key="expenseReimburseList"
          :params="{
            ...query,
          }"
          size="mini"
          type="warning"
          v-permission="crud.permission.print"
        />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { ref, inject } from 'vue'

import { dateQueryTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const subjectList = ref([])
const totalAmount = ref(0)
const expenseList = inject('expenseList')

const defaultQuery = {
  expenseTypeId: undefined,
  expenseSubjectId: undefined,
  dateQueryTypeEnum: dateQueryTypeEnum.YEAR.V,
  date: undefined,
  month: undefined,
  payee: undefined,
  reimbursementPerson: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)
function fetchChange(val) {
  subjectList.value = expenseList.value.find((v) => v.id === val)?.links
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  totalAmount.value = data.content.reduce((prev, curr) => {
    const value = Number(curr.reimburseAmount)
    if (!isNaN(value)) {
      return prev + value
    } else {
      return prev
    }
  }, 0)
}
</script>

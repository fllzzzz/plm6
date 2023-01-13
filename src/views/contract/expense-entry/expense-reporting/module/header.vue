<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
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
      />
      <common-select
        v-model="query.expenseTypeId"
        :options="expenseList"
        type="other"
        :data-structure="{ key: 'id', label: 'name', value: 'id' }"
        class="filter-item"
        clearable
        style="width: 200px"
        placeholder="费用类别"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.expenseSubjectId"
        :options="dict.reimbursement_type"
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
        placeholder="报销人"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag effect="plain" class="filter-item" size="medium">
          <span> 报销总额（元）：{{ summaryData.info }}</span>
        </el-tag>
        <print-table api-key="expenseReimburseList" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { inject } from 'vue'
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import { timeTypeEnum } from '@enum-ms/contract'
import useDict from '@compos/store/use-dict'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const dict = useDict(['reimbursement_type'])

const summaryData = inject('summaryData')
const expenseList = inject('expenseList')
const defaultQuery = {
  timeType: timeTypeEnum.ALL_YEAR.V,
  expenseTypeId: undefined,
  expenseSubjectId: undefined,
  year: parseTime(new Date(), '{y}'),
  month: undefined
}

const { crud, query } = regHeader(defaultQuery)

function handleChange(val) {
  console.log(val, 'val')
  if (val === timeTypeEnum.ALL_YEAR.V) {
    query.year = parseTime(new Date(), '{y}')
    query.month = undefined
  } else {
    query.year = parseTime(new Date(), '{y}')
    query.month = parseTime(new Date(), '{m}')
  }
  crud.toQuery()
}
// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>

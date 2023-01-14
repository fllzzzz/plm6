<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 100px !important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.wageType"
        :options="managementSalaryTypeEnum.ENUM"
        class="filter-item"
        type="enum"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table v-permission="crud.permission.print" api-key="managementSalaryList" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import { managementSalaryTypeEnum } from '@enum-ms/contract'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  wageType: managementSalaryTypeEnum.MANAGEMENT_SALARY.V
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style lang="scss" scoped>
</style>

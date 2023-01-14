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
      <crudOperation>
        <template #viewLeft>
          <print-table api-key="propertyFeeList" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
        </template>
      </crudOperation>
    </div>
  </div>
</template>
<script setup>
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  year: parseTime(new Date(), '{y}')
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style lang="scss" scoped>
</style>

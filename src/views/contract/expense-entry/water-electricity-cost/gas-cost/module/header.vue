<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="query.year"
          type="year"
          size="small"
          class="date-item filter-item"
          style="width: 100px !important"
          placeholder="选择年"
          format="YYYY"
          :clearable="false"
          value-format="YYYY"
          :disabled-date="disabledDate"
          @change="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <el-tag v-if="props.rowDetail?.levelName" size="medium" effect="plain" type="warning" class="filter-item">
          {{ props.rowDetail?.levelName }}
        </el-tag>
        <print-table v-permission="crud.permission.print" api-key="gasRecord" :params="{ year: query.year }" size="mini" type="warning" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { defineProps } from 'vue'

import { parseTime } from '@/utils/date'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const props = defineProps({
  rowDetail: {
    type: Object,
    default: () => {}
  }
})

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  classifyId: undefined
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
const { crud, query } = regHeader(defaultQuery)
</script>

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
        :clearable="false"
        value-format="YYYY"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.classifyId"
        :options="gasTypeList"
        class="filter-item"
        type="other"
        :data-structure="{ key: 'id', label: 'name', value: 'id' }"
        @change="handleChange"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table api-key="gasRecord" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { defineEmits, defineProps } from 'vue'
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const emit = defineEmits(['change'])
const prop = defineProps({
  gasTypeList: {
    type: Array,
    default: () => []
  }
})
const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  classifyId: prop.gasTypeList[0]?.id
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
const { crud, query } = regHeader(defaultQuery)

function handleChange(val) {
  crud.toQuery()
  const unit = prop.gasTypeList.find((v) => v.id === val)?.accountingUnit
  const gasType = prop.gasTypeList.find((v) => v.id === val)?.name
  emit('change', unit, gasType)
}
</script>

<style lang="scss" scoped>
</style>

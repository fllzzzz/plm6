<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入单体名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag type="info" effect="plain" v-if="props.globalProject.startDate" style="margin-right:5px;">
          <span v-if="globalProject.startDate">开工日期:{{parseTime(globalProject.startDate,'{y}-{m}-{d}')}}</span>
          <span v-if="globalProject.endDate"> | 完成日期:{{parseTime(globalProject.endDate,'{y}-{m}-{d}')}}</span>
          <span v-if="globalProject.startDate && globalProject.endDate"> | 工期:{{dateDifference(globalProject.startDate,globalProject.endDate)}}天</span>
        </el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { parseTime, dateDifference } from '@/utils/date'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  }
})
</script>

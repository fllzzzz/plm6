<template>
  <div class="project-chart">
    <div class="chart-head">
       <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="YYYY"
        format="YYYY"
        :disabled-date="disabledDate"
        @change="handleYearChange"
      />
    </div>
    <div v-loading="projectInfo.loading" class="chart-container" :style="{height: maxHeight + 'px'}">
      <chart id="projectChart" width="300px" />
    </div>
  </div>
</template>

<script setup>
import { ref, inject, defineEmits, onMounted } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import chart from './module/chart'

const emit = defineEmits(['update:year'])

const projectInfo = inject('projectInfo')

const year = ref(parseTime(new Date(), '{y}'))

const { maxHeight } = useMaxHeight(
  {
    extraBox: '.chart-head',
    wrapperBox: '.chart-container',
    // 右侧最小宽度 - 顶部时间选择框
    minHeight: 566,
    extraHeight: 40
  }
)

onMounted(() => {
  // 默认选择当年
  handleYearChange(year.value)
})

function disabledDate(time) {
  return time > new Date()
}

function handleYearChange(val) {
  emit('update:year', val)
}
</script>

<style lang="scss" scoped>
.project-chart {
    width: 340px;
    padding: 20px 0 20px 20px;
    overflow: hidden;
    border-right: 1px solid #ededed;
    .chart-head {
        padding-right: 20px;
        padding-bottom: 20px;
    }
    .chart-container {
        width: 319px;
        padding-right: 20px;
        overflow-y: auto;
    }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>


<template>
  <div class="project-chart">
    <div class="chart-head">
       <common-select
        v-model="productionLine"
        :options="productionList"
        clearable
        type="other"
        :data-structure="{ key: 'productionLine', label: 'productionLine', value: 'productionLine' }"
        placeholder="请选择产线"
        class="filter-item"
      />
        <!-- <production-line-select
           ref="productionLineRef"
          v-model="productionLineId"
          :factory-id="factoryId"
          placeholder="请先选择生产线"
          style="width: 200px"
          defaultValue /> -->
      <span class="filter-item" style="font-size: 14px; align-self: center">单位：件/吨</span>
    </div>
    <div v-loading="projectInfo.loading" v-permission="permission.statistics" class="chart-container" :style="{ height: maxHeight + 'px' }">
      <chart style="border: 1px solid #ededed" id="projectChart" width="300px" v-if="route.name !== 'PlanProject'" @success="handleEchartsChange" />
    </div>
  </div>
</template>

<script setup>
import { ref, inject, defineEmits, onMounted } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import chart from './module/chart'
import { useRoute } from 'vue-router'

const emit = defineEmits(['update:year', 'change', 'success'])
const route = useRoute()
const projectInfo = inject('projectInfo')
const permission = inject('permission')

const year = ref(parseTime(new Date(), '{y}'))

const productionLine = ref()
const productionList = [{ productionLine: '一线' }, { productionLine: '二线' }, { productionLine: '三线' }]
const { maxHeight } = useMaxHeight({
  extraBox: ['.chart-head'],
  wrapperBox: ['.chart-container'],
  // 右侧最小宽度 - 顶部时间选择框
  minHeight: 566,
  extraHeight: 115
})

onMounted(() => {
  // 默认选择当年
  handleYearChange(year.value)
})

function handleYearChange(val) {
  emit('update:year', val)
}

function handleEchartsChange(val) {
  emit('success', val)
}

</script>

<style lang="scss" scoped>
.project-chart {
  width: 340px;
  padding: 0 0 20px 0;
  overflow: hidden;
  border-right: 1px solid #ededed;
  .chart-head {
    display: flex;
    justify-content: space-between;
    padding-right: 20px;
    padding-bottom: 10px;
  }
  .chart-container {
    width: 320px;
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


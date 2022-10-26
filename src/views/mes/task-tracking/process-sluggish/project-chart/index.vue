<template>
  <div class="project-chart">
    <div class="chart-head">
      <tag-tabs
        v-model="productionLineId"
        class="filter-item"
        :style="'width:calc(100% - 100px)'"
        :data="productionLineList"
        :itemKey="'workshopId'"
        @change="tabChange"
      >
        <template #default="{ item }">
          <span>{{ item.productionLine }}</span>
        </template>
      </tag-tabs>
      <span class="filter-item" style="width: 80px;font-size: 14px; align-self: center">单位：件/吨</span>
    </div>
    <div v-loading="projectInfo.loading" v-permission="permission.statistics" class="chart-container" :style="{ height: maxHeight + 'px' }">
      <chart
        id="projectChart"
        width="300px"
        @success="handleEchartsChange"
      />
    </div>
  </div>
</template>

<script setup>
import { ref, inject, defineEmits, onMounted } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import chart from './module/chart'
import tagTabs from '@comp-common/tag-tabs'

const emit = defineEmits(['update:year', 'change', 'success'])

const projectInfo = inject('projectInfo')
const permission = inject('permission')

const year = ref(parseTime(new Date(), '{y}'))

const productionLineId = ref()
const productionLineList = [
  { workshopId: 1, productionLine: '一线' },
  { workshopId: 2, productionLine: '二线' },
  { workshopId: 3, productionLine: '三线' }
]
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


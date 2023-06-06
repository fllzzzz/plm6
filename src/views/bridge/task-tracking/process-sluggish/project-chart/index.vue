<template>
  <div class="project-chart">
    <div class="chart-head">
      <div style="width: 100%;display: flex;justify-content: flex-end; margin-bottom: 8px">
        <span style="font-size: 14px">单位：件|吨</span>
      </div>
      <div style="width: calc(100% - 0px)">
        <tag-tabs
          v-if="productionLineList?.length"
          v-model="productionLineId"
          class="filter-item"
          style="width: 100%"
          :data="productionLineList"
          itemKey="id"
          @change="tabChange"
        >
          <template #default="{ item }">
            <span>{{ item.name }}</span>
          </template>
        </tag-tabs>
      </div>
    </div>
    <div v-permission="permission.statistics" class="chart-container" :style="{ height: maxHeight + 'px' }">
      <chart id="projectChart" width="300px" @success="handleEchartsChange" />
    </div>
  </div>
</template>

<script setup>
import { productionLineProcess } from '@/api/bridge/bridge-task-tracking/process-sluggish.js'
import { ref, inject, defineEmits, onMounted, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import chart from './module/chart'
import tagTabs from '@comp-common/tag-tabs'

const emit = defineEmits(['update:year', 'change', 'success'])
const permission = inject('permission')

const productionLineId = ref()
const productionLineList = ref([])

const productType = inject('searchProductType')
const workShopId = inject('workShopId')

watch(
  () => productType.value,
  (val) => {
    if (val) {
      productionLineGet()
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => workShopId.value,
  (val) => {
    if (val) {
      productionLineGet()
      tabChange()
    }
  },
  { deep: true, immediate: true }
)

async function productionLineGet() {
  productionLineList.value = []
  productionLineId.value = undefined
  if (!productType.value || !workShopId.value) {
    return
  }
  try {
    const data = await productionLineProcess({
      productType: productType.value,
      workShopId: workShopId.value
    })
    productionLineList.value = data
    productionLineId.value = data[0]?.id
  } catch (e) {
    console.log('获取当前车间下的生产线失败', e)
  } finally {
    tabChange(productionLineId.value)
  }
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.chart-head'],
  wrapperBox: ['.chart-container'],
  // 右侧最小宽度 - 顶部时间选择框
  minHeight: 566,
  extraHeight: 115
})

onMounted(() => {
  productionLineGet()
})

function tabChange(val) {
  emit('change', val)
  emit('success', {})
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
  .chart-head {
    width: 100%;
    // display: flex;
    // justify-content: space-between;
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


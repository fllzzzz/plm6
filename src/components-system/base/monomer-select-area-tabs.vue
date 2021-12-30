<template>
  <monomer-select ref="monomerRef" v-model="monomerId" :project-id="projectId" class="filter-item" @getAreaInfo="getAreaInfo" />
  <area-tabs
    class="filter-item"
    style="width: calc(100% - 230px)"
    v-model="areaId"
    :area-info="areaInfo"
    :default-tab="defaultTab"
    @tab-click="tabClick"
  />
</template>

<script setup>
import { ref, defineProps, defineExpose, defineEmits } from 'vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'

const emit = defineEmits(['change'])
defineProps({
  projectId: {
    type: [Number, String]
  }
})

const monomerRef = ref()
const monomerId = ref()
const areaId = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  emit('change', { monomerId: monomerId, areaId: areaId, currentArea: currentArea })
}

function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: `${areaInfo.value[0].name}(${areaInfo.value[0].axis})`
    }
  } else {
    defaultTab.value = {}
  }
}

// 单体绑定的类型
function getProductType() {
  return monomerRef.value?.getProductType(monomerId.value) || {}
}

defineExpose({
  getProductType
})
</script>

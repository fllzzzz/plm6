<template>
<div style="display: flex">
  <monomer-select
    ref="monomerRef"
    v-model="monomerId"
    :productType="proType"
    :project-id="projectId"
    class="filter-item"
    :default="false"
    style="width: 200px"
    @getAreaInfo="getAreaInfo"
  />
  <area-tabs
    class="filter-item"
    style="width:calc(100% - 206px);marginRight: 0px"
    v-model="areaId"
    :area-info="areaInfo"
    :default-tab="defaultTab"
    @tab-click="tabClick"
  />
  </div>
</template>

<script setup>
import { ref, defineProps, defineExpose, defineEmits, computed } from 'vue'

import { convertProductType } from '@/utils/mes/convert-product-type'

import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'

const emit = defineEmits(['change'])
const props = defineProps({
  projectId: {
    type: [Number, String]
  },
  productType: {
    type: [Number, String, undefined]
  },
  category: {
    type: [Number, String, undefined]
  },
  needConvert: {
    type: Boolean,
    default: false
  }
})

const proType = computed(() => {
  return props.needConvert ? convertProductType(props.productType, props.category) : props.productType
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
  emit('change', { monomerId: monomerId.value, areaId: areaId.value, currentArea: currentArea })
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

<template>
  <monomer-select
    ref="monomerRef"
    v-model="copyMonomerId"
    :productType="proType"
    :clearable="clearable"
    :project-id="projectId"
    :disabled="monomerDisabled"
    :default="monomerDefault"
    :filterArea="false"
    class="filter-item"
    @getAreaInfo="getAreaInfo"
  />
  <slot name="middle"></slot>
  <common-select
    v-model="copyAreaId"
    :options="areaList"
    size="small"
    :default="areaDefault"
    :disabled="areaDisabled"
    :multiple="areaMultiple"
    :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
    :clearable="areaClearable"
    :noDataText="copyMonomerId ? '暂无数据' : '未选择单体'"
    class="filter-item"
    placeholder="请选择区域"
    style="width: 200px"
    @change="handleChange"
  />
</template>

<script setup>
import { ref, defineProps, defineExpose, defineEmits, computed, watch } from 'vue'

import { convertProductType } from '@/utils/mes/convert-product-type'

import monomerSelect from '@/components-system/plan/monomer-select'

const emit = defineEmits(['change', 'update:monomerId', 'update:areaId', 'monomerChange', 'areaChange'])
const props = defineProps({
  monomerId: {
    type: [Number, String, undefined]
  },
  areaId: {
    type: [Number, String, Array, undefined]
  },
  projectId: {
    type: [Number, String, undefined]
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
  },
  clearable: {
    type: Boolean,
    default: false
  },
  areaClearable: {
    type: Boolean,
    default: false
  },
  monomerDefault: {
    type: Boolean,
    default: false
  },
  areaDefault: {
    type: Boolean,
    default: false
  },
  monomerDisabled: {
    type: Boolean,
    default: false
  },
  areaDisabled: {
    type: Boolean,
    default: false
  },
  filterArea: {
    type: Boolean,
    default: true
  },
  areaMultiple: {
    type: Boolean,
    default: false
  }
})

const proType = computed(() => {
  return props.needConvert ? convertProductType(props.productType, props.category) : props.productType
})

const monomerRef = ref()
const copyMonomerId = ref()
const copyAreaId = ref()
const areaList = ref([])

watch(
  () => props.monomerId,
  (value) => {
    copyMonomerId.value = value
  },
  { immediate: true }
)

watch(
  () => props.areaId,
  (value) => {
    copyAreaId.value = value
  },
  { immediate: true }
)

function handleChange(val) {
  emit('update:areaId', copyAreaId.value)
  emit('areaChange')
  emit('change', { monomerId: copyMonomerId.value, areaId: copyAreaId.value })
}

function getAreaInfo(val, monomerValue) {
  console.log(val, 'getAreaInfo')
  if (props.filterArea) {
    areaList.value = val && val.filter((v) => v.productType & proType.value)
  }
  areaList.value = val || []
  emit('update:monomerId', monomerValue)
  emit('monomerChange')
  emit('change', { monomerId: copyMonomerId.value })
}

// 单体绑定的类型
function getProductType() {
  return monomerRef.value?.getProductType(copyMonomerId.value) || {}
}

defineExpose({
  getProductType
})
</script>

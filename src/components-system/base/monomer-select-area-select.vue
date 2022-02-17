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
    :options="areaOptions"
    size="small"
    :default="areaDefault"
    :disabled="areaDisabled"
    :multiple="areaMultiple"
    :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
    :clearable="areaClearable"
    :noDataText="copyMonomerId ? '暂无数据' : '未选择单体'"
    class="filter-item"
    placeholder="请选择区域"
    style="width: 250px"
    @change="handleChange"
  />
</template>

<script setup>
import { ref, defineProps, defineExpose, defineEmits, computed, watch } from 'vue'

import { convertProductType } from '@/utils/mes/convert-product-type'

import monomerSelect from '@/components-system/plan/monomer-select'

const emit = defineEmits(['change', 'update:monomerId', 'update:areaId'])
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

const areaOptions = computed(() => {
  return areaList.value && areaList.value.filter((v) => v.productType & proType.value)
})

function handleChange(val) {
  emit('update:areaId', copyAreaId.value)
  emit('change', { monomerId: copyMonomerId.value, areaId: copyAreaId.value })
}

function getAreaInfo(val) {
  areaList.value = val || []
  emit('update:monomerId', copyMonomerId.value)
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

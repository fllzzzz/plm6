<template>
  <common-radio-button
    ref="commonRadioButtonRef"
    v-model="selectId"
    :options="options"
    :size="size"
    :clearable="clearable"
    :disabledVal="disabledVal"
    default
  ></common-radio-button>
</template>

<script setup>
import { defineExpose, defineProps, defineEmits, watch, computed, ref, nextTick } from 'vue'
import { isNotBlank } from '@data-type/index'
import RAF from '@/utils/raf'

import useProcess from '@compos/store/use-process'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  // 查询指定产品类型，不传查所有
  productType: {
    // Value
    type: Number,
    default: undefined
  },
  // eslint-disable-next-line vue/require-default-prop
  modelValue: {
    type: [Number, Array]
  },
  clearable: {
    type: Boolean,
    default: false
  },
  size: {
    type: String,
    default: 'small'
  },
  disabledVal: {
    type: Array,
    default: () => []
  }
})

const commonRadioButtonRef = ref()
const hasLoad = ref(false)
const selectId = ref()

const { process } = useProcess()

const options = computed(() => {
  return (
    (process.value &&
      process.value.length &&
      process.value.filter((v) => {
        return isNotBlank(props.productType) && props.productType === v.productType
      })) ||
    []
  )
})

const optionMap = computed(() => {
  const _map = {}
  process.value.forEach(v => {
    _map[v.id] = v
  })
  return _map
})

watch(
  () => props.productType,
  () => {
    nextTick(() => {
      // 重置选择
      commonRadioButtonRef.value.selectChange(0)
    })
  },
  { immediate: true }
)

watch(
  () => props.modelValue,
  (val) => {
    selectId.value = val
  },
  { immediate: true }
)

watch(
  () => selectId.value,
  (val) => {
    handleChange(val)
  },
  { immediate: true }
)

function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

async function getSourceData() {
  if (!hasLoad.value) {
    await waitLoad()
  }
  return process.value
}

function getOptionInfo(id) {
  return optionMap.value[id]
}

async function waitLoad() {
  return new Promise((resolve, reject) => {
    RAF.setInterval(() => {
      if (hasLoad.value) {
        RAF.clearInterval()
        resolve()
      }
    }, 100)
  })
}

defineExpose({
  getSourceData,
  getOptionInfo
})
</script>

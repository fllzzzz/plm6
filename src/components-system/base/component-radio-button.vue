<template>
  <common-radio-button
    v-show="!isHide"
    v-model="selectValue"
    :options="options"
    :type="type"
    :unshowVal="unshowAllVal"
    :disabledVal="disabledVal"
    :showOptionAll="showOptionAll"
    :default="props.default"
    @change="handleChange"
  >
    <template #suffix="{ item }">
      <slot name="suffix" :item="item"></slot>
    </template>
  </common-radio-button>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import { componentTypeEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'
import { uniqueArr } from '@/utils/data-type/array'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: [Number, String, Boolean, undefined],
  size: {
    type: String,
    default: 'small'
  },
  options: {
    type: [Object, Array, Number],
    required: true
  },
  default: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  disabledVal: {
    type: Array,
    default: () => []
  },
  unshowVal: {
    type: Array,
    default: () => []
  },
  type: {
    // dict , enum, other
    type: String,
    default: 'other'
  },
  showOptionAll: {
    type: Boolean,
    default: false
  }
})

const { productMenu } = mapGetters('productMenu')

const unshowProductVal = computed(() => {
  const _unshowArr = []
  for (const item in componentTypeEnum.ENUM) {
    const value = componentTypeEnum.KV[item]
    if (!(value & productMenu.value)) {
      _unshowArr.push(value)
    }
  }
  return _unshowArr
})

const unshowAllVal = computed(() => {
  return uniqueArr([...props.unshowVal, ...unshowProductVal.value])
})

const isHide = computed(() => {
  // const optionsLength = Object.keys(props.options).length
  const _showVal = []
  for (const item in props.options) {
    const value = props.options[item].V
    if (unshowAllVal.value.indexOf(value) === -1) {
      _showVal.push(value)
    }
  }
  return !_showVal.length || _showVal.length <= 1
})

const selectValue = ref()

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

function handleChange(val) {
  let data = val
  if (isBlank(data)) data = undefined
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data)
  }
}
</script>

<style lang="scss" scoped></style>

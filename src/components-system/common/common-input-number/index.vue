<template>
  <el-input-number
    ref="inputRef"
    :key="`input_number_${Math.random()}`"
    v-model="copyValue"
    :min="min"
    :max="max"
    :step="step"
    :step-strictly="stepStrictly"
    :precision="precision"
    :size="size"
    :disabled="disabled"
    :controls="controls"
    :controls-position="controlsPosition"
    :name="name"
    :label="label"
    :placeholder="placeholder"
    @change="changeCallBack"
    @blur="blurCallBack"
    @focus="focusCallBack"
  />
</template>

<script setup>
import { isNotBlank } from '@/utils/data-type'
import { ref, defineExpose, defineProps, defineEmits, watchEffect } from 'vue'

const emit = defineEmits(['change', 'blur', 'focus'])

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  // TODO:是否限制类型
  value: {
    type: [Number, null]
  },
  min: {
    type: Number,
    default: undefined
  },
  max: {
    type: Number,
    default: undefined
  },
  step: {
    type: Number,
    default: 1
  },
  stepStrictly: {
    type: Boolean,
    default: false
  },
  precision: {
    type: Number,
    default: 0
  },
  size: {
    type: String,
    default: undefined
  },
  disabled: {
    type: Boolean,
    default: false
  },
  controls: {
    type: Boolean,
    default: true
  },
  controlsPosition: {
    type: String,
    default: undefined
  },
  name: {
    type: String,
    default: undefined
  },
  label: {
    type: String,
    default: undefined
  },
  placeholder: {
    type: String,
    default: undefined
  }
})

const inputRef = ref()
const copyValue = ref()

watchEffect(() => {
  copyValue.value = !isNaN(props.modelValue) && isNotBlank(props.modelValue) ? props.modelValue : undefined
})

function changeCallBack(currentValue, oldValue) {
  emit('change', currentValue, oldValue)
}
function blurCallBack(event) {
  emit('blur', event)
}
function focusCallBack(event) {
  emit('focus', event)
}
function focus() {
  inputRef.value.focus()
}
function select() {
  inputRef.value.select()
}

defineExpose({
  focus,
  select
})
</script>

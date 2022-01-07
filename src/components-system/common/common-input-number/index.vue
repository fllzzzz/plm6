<template>
  <el-input-number
    ref="inputRef"
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
import { ref, defineExpose, defineProps, defineEmits, watchEffect, computed } from 'vue'

const emit = defineEmits(['change', 'blur', 'focus', 'update:modelValue'])

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  modelValue: {
    type: [Number, String, null]
  },
  min: {
    type: [String, Number],
    default: undefined
  },
  max: {
    type: [String, Number],
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
    type: Number
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

const max = computed(() => {
  if (typeof props.max === 'string') return +props.max
  return props.max
})

const min = computed(() => {
  if (typeof props.min === 'string') return +props.min
  return props.min
})

watchEffect(() => {
  let value = !isNaN(props.modelValue) && isNotBlank(props.modelValue) ? props.modelValue : undefined
  if (typeof value === 'string') value = +value
  copyValue.value = value
})

watchEffect(() => {
  if (isNotBlank(copyValue.value) && isNotBlank(props.max)) {
    if (copyValue.value > props.max) {
      changeCallBack(props.max, copyValue.value)
    }
  }
})

watchEffect(() => {
  if (isNotBlank(copyValue.value) && isNotBlank(props.max)) {
    if (copyValue.value < props.min) {
      changeCallBack(props.min, copyValue.value)
    }
  }
})

function changeCallBack(currentValue, oldValue) {
  emit('update:modelValue', currentValue)
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

function blur() {
  inputRef.value.blur()
}

defineExpose({
  focus,
  blur
})
</script>

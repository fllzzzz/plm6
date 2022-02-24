<template>
  <el-input-number
    v-if="mode !== 'input'"
    v-bind="$attrs"
    ref="inputRef"
    v-model="currentValue"
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
  <el-input
    v-else
    v-bind="$attrs"
    ref="inputRef"
    v-model="currentValue"
    type="number"
    clearable
    :size="size"
    :disabled="disabled"
    :placeholder="placeholder"
    @change="changeCallBack"
    @blur="blurCallBack"
    @focus="focusCallBack"
  />
</template>

<script setup>
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { ref, defineExpose, defineProps, defineEmits, watchEffect, computed } from 'vue'

const emit = defineEmits(['change', 'blur', 'focus', 'update:modelValue'])

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  modelValue: {
    type: [Number, String, null]
  },
  mode: {
    // 模式，普通模式，input模式
    type: String
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
const currentValue = ref()

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
  currentValue.value = value
})

watchEffect(() => {
  if (isNotBlank(currentValue.value) && isNotBlank(props.max)) {
    if (currentValue.value > props.max) {
      changeCallBack(props.max, currentValue.value)
    }
  }
})

watchEffect(() => {
  if (isNotBlank(currentValue.value) && isNotBlank(props.max)) {
    if (currentValue.value < props.min) {
      changeCallBack(props.min, currentValue.value)
    }
  }
})

function changeCallBack(currentValue, oldValue) {
  emit('update:modelValue', currentValue)
  emit('change', currentValue, oldValue)
}
function blurCallBack(event) {
  if (props.mode === 'input') setCurrentValue()
  emit('blur', event)
}

function setCurrentValue() {
  const oldValue = currentValue.value
  let val = currentValue.value
  if (typeof val !== 'number') {
    val = void 0
  }
  if (typeof val === 'number' && props.precision !== void 0) {
    val = toPrecision(val, props.precision)
  }
  if (val !== void 0 && val >= props.max) val = props.max
  if (val !== void 0 && val <= props.min) val = props.min
  if (val === currentValue.value) return
  currentValue.value = val
  changeCallBack(currentValue.value, oldValue)
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

<!-- 变更原因:下拉选择框 -->
<template>
  <common-select
    v-bind="$attrs"
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :loading="!loaded"
    :clearable="clearable"
    :showOptionAll="showOptionAll"
    :allLabelText="'全部'"
    filterable
    :placeholder="placeholder"
    :options="changeReasonConfig"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import useChangeReason from '@compos/store/use-change-reason'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  collapseTags: {
    type: Boolean,
    default: false
  },
  showOptionAll: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择变更原因'
  }
})

const selectValue = ref()

const { loaded, changeReasonConfig } = useChangeReason()

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

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
    :options="subcontractType"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import useSubcontractType from '@compos/store/use-subcontract-type'

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
    default: '请选择分包类别'
  }
})

const selectValue = ref()

const { loaded, subcontractType } = useSubcontractType()

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

<!-- 工厂:下拉选择框 -->
<template>
  <common-select
    v-bind="$attrs"
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    :showOptionAll="showOptionAll"
    :allLabelText="'全部工厂'"
    filterable
    :placeholder="placeholder"
    :options="factories"
    :only-one-default="onlyOneDefault"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import useFactory from '@compos/store/use-factories'

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
    default: '请选择工厂'
  },
  // 当只有一个工厂时，默认选中
  onlyOneDefault: {
    type: Boolean,
    default: true
  }
  // TODO: 默认选中当前用户所属工厂
})

const selectValue = ref()

const { loaded, factories } = useFactory()

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

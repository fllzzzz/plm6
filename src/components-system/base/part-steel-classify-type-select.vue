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
    :allLabelText="'钢材种类'"
    filterable
    :placeholder="placeholder"
    :options="options"
    :only-one-default="onlyOneDefault"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'
import useSteelClassifyConf from '@/composables/store/use-steel-material-classify'

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
    default: '钢材种类'
  },
  // 当只有一个钢材种类时，默认选中
  onlyOneDefault: {
    type: Boolean,
    default: true
  }
})

const selectValue = ref()

const { loaded, steelClassifyConf: options } = useSteelClassifyConf()

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
    // 获取 配置详情
    emit('change', data)
  }
}
</script>

<!-- 工厂:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    :options="factories"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'
import useFactory from '@compos/store/use-factories'

const emit = defineEmits(['change', 'update:value'])

const props = defineProps({
  value: {
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
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择工厂'
  }
})

const selectValue = ref()

const { loaded, factories } = useFactory(loadedCallBack)

watch(
  () => props.value,
  (value) => {
    selectValue.value = value
    // 有默认值的情况，并且value为空，则给value赋值
    if (props.default && isBlank(value) && isNotBlank(factories.value)) {
      selectValue.value = factories.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

function handleChange(val) {
  if (props.value !== val) {
    emit('update:value', val)
    emit('change', val)
  }
}

function loadedCallBack() {
  if (isNotBlank(factories.value) && props.default && !selectValue.value) {
    selectValue.value = factories.value[0].value
  }
  handleChange(selectValue.value)
}

</script>
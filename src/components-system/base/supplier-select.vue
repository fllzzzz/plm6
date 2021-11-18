<!-- 供应商:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    :filterable="filterable"
    :placeholder="placeholder"
    :options="options"
    @change="handleChange"
  >
  </common-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { supplierTypeEnum } from '@/utils/enum/modules/supplier'
import { isNotBlank, isBlank } from '@data-type/index'
import useSuppliers from '@compos/store/use-suppliers'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String]
  },
  basicClass: {
    type: Number
  },
  type: {
    type: Number,
    default: supplierTypeEnum.RAW_MATERIAL.V | supplierTypeEnum.MANUFACTURED.V
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
  filterable: {
    type: Boolean,
    default: true
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
    default: '请选择供应商'
  }
})

const selectValue = ref()

const { loaded, suppliers } = useSuppliers(loadedCallBack)

const options = computed(() => {
  if (props.basicClass) {
    return suppliers.value.filter(v => v.basicClass & props.basicClass)
  }
  if (props.type) {
    return suppliers.value.filter(v => v.type & props.type)
  }
  return suppliers.value
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    // 有默认值的情况，并且value为空，则给value赋值
    if (props.default && isBlank(value) && isNotBlank(suppliers.value)) {
      selectValue.value = suppliers.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

function handleChange(val) {
  if (props.modelValue !== val) {
    emit('update:modelValue', val)
    emit('change', val)
  }
}

function loadedCallBack() {
  if (isNotBlank(suppliers.value) && props.default && !selectValue.value) {
    selectValue.value = suppliers.value[0].value
  }
  handleChange(selectValue.value)
}
</script>

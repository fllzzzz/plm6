<!-- 申购单/采购申请单:下拉选择框 -->
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
    :options="options"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'
import useUnclosedRequisition from '@compos/store/use-unclosed-requisition'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String]
  },
  basicClass: {
    type: Number
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
    default: '选择申购单'
  }
})

const selectValue = ref()

const { loaded, requisitions } = useUnclosedRequisition()

const options = computed(() => {
  if (props.basicClass) {
    return requisitions.value.filter(v => v.basicClass & props.basicClass)
  }
  return requisitions.value
})

watch(
  options,
  (opt) => {
    loadedCallBack()
  }
)

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    // 有默认值的情况，并且value为空，则给value赋值
    if (props.default && isBlank(value) && isNotBlank(options.value)) {
      selectValue.value = options.value[0].value
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
  if (isNotBlank(options.value) && props.default && !selectValue.value) {
    selectValue.value = options.value[0].value
  } else {
    selectValue.value = undefined
  }
  handleChange(selectValue.value)
}

</script>

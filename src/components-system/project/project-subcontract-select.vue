<!-- 分包项目列表-->
<template>
  <common-select
    ref="subcontractRef"
    v-model="selectValue"
    :loading="loading"
    :options="options"
    :type="'other'"
    :collapse-tags="collapseTags"
    :data-structure="dataStructure"
    :size="size"
    :clearable="clearable"
    :default-id="props.defaultId"
    :disabled="props.disabled"
    filterable
    :placeholder="placeholder"
    @change="selectChange"
  />
</template>

<script setup>
import { ref, defineProps, defineEmits, computed, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import useUserProjects from '@compos/store/use-user-projects'
import { isBlank, judgeSameValue } from '@data-type/index'

const { projects } = useUserProjects()

const subcontractRef = ref()
const emit = defineEmits(['update:modelValue', 'change', 'changeInfo'])
const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  dataStructure: {
    type: Object,
    default: () => {
      return { key: 'id', label: 'name', value: 'id' }
    }
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
  defaultId: {
    type: [Number, String],
    default: undefined
  },
  placeholder: {
    type: String,
    default: '请选择项目'
  },
  // 项目业务类型
  businessType: {
    type: Number,
    default: businessTypeEnum.INSTALLATION.V
  }
})

const loading = ref(false)
const selectValue = ref()

const options = computed(() => {
  // 过滤出项目承包项目
  return projects.value.filter(v => {
    return v.businessType === props.businessType
  })
})

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  }
)

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

function selectChange(val) {
  let data = val
  if (isBlank(data)) data = undefined
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data)
    const choseVal = val ? options.value.find(v => v.id === val) : {}
    emit('changeInfo', choseVal)
  }
}
</script>

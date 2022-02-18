<!-- 可签证项目列表（过滤已有结算单的项目） -->
<template>
  <common-select
    v-model="selectValue"
    :loading="loading"
    :options="options"
    :type="'other'"
    :collapse-tags="collapseTags"
    :dataStructure="{ key: 'id', label: 'shortName', value: 'id' }"
    :size="size"
    :clearable="clearable"
    :default="props.default"
    :disabled="props.disabled"
    filterable
    :placeholder="placeholder"
    @change="selectChange"
  />
</template>

<script setup>
import { ref, defineProps, defineEmits, computed, watch } from 'vue'
import { useStore } from 'vuex'

import { businessTypeEnum } from '@enum-ms/contract'
import useUserVisaProjects from '@compos/store/use-user-visa-projects'

const emit = defineEmits(['update:modelValue', 'change'])
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
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择项目'
  },
  // 过滤有结算记录的项目（结算未审核也过滤）
  filterSettlement: {
    type: Boolean,
    default: false
  },
  // 项目业务类型
  businessType: {
    type: Number,
    default: businessTypeEnum.MACHINING.V
  }
})

const loading = ref(false)
const selectValue = ref()

const store = useStore()

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  },
  { immediate: true }
)

// 项目业务类型
watch(
  () => props.businessType,
  (val) => {
    store.dispatch('project/fetchUserVisaProjects', { businessType: val })
  }
)

const { visaProjects } = useUserVisaProjects({ businessType: props.businessType })

const options = computed(() => {
  // 是否过滤有结算记录的项目
  return visaProjects.value.filter(v => {
    // isSubmitSettle === true 表示该项目有结算记录
    return !props.filterSettlement || props.filterSettlement === !v.isSubmitSettle
  })
})

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

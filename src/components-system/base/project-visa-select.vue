<!-- 可签证项目列表（过滤已有结算单的项目） -->
<template>
  <common-select
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
import { useStore } from 'vuex'

import { businessTypeEnum } from '@enum-ms/contract'
import useUserVisaProjects from '@compos/store/use-user-visa-projects'

const emit = defineEmits(['update:modelValue', 'change', 'projectChange'])
const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  dataStructure: {
    type: Object,
    default: () => {
      return { key: 'id', label: 'fullName', value: 'id' }
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
  // 过滤有结算记录的项目（结算未审核也过滤）
  filterSettlement: {
    type: Boolean,
    default: false
  },
  // 是否刷新
  isRefresh: {
    type: Boolean,
    default: false
  },
  // 项目业务类型
  businessType: {
    type: Number,
    default: businessTypeEnum.MACHINING.V
  },
  projectStatus: {
    type: Number,
    default: undefined
  },
  saveSettlement: {
    type: Boolean,
    default: false
  }
})

const loading = ref(false)
const selectValue = ref()

const store = useStore()

const { visaProjects } = useUserVisaProjects()

const options = computed(() => {
  const filterData = visaProjects.value.filter(v => v.businessType === props.businessType)
  let projectData = []
  if (props.projectStatus) {
    projectData = filterData.filter(v => v.status === props.projectStatus)
  } else {
    projectData = filterData
  }
  if (props.saveSettlement) {
    projectData = projectData.filter(v => v.isSubmitSettle === true)
  }
  // 是否过滤有结算记录的项目
  return projectData.filter(v => {
    // isSubmitSettle === true 表示该项目有结算记录
    return !props.filterSettlement || props.filterSettlement === !v.isSubmitSettle
  })
})

// 默认值是否可用
const flag = computed(() => {
  return options.value.map(v => v.id).includes(props.defaultId)
})

// 设置默认值
watch(
  () => flag.value,
  (bol) => {
    const id = bol ? props.defaultId : void 0
    selectChange(id)
  },
  { immediate: true }
)

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  }
)

// 是否刷新
watch(
  () => props.isRefresh,
  (val) => {
    if (val) {
      fetchProjects()
    }
  }
)

// 重新获取签证项目
function fetchProjects() {
  store.dispatch('project/fetchUserVisaProjects')
}

// change
function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
  const findVal = options.value.find(v => v.id === val) || {}
  emit('projectChange', findVal)
}
</script>

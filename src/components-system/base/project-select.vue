<!-- 可签证项目列表（过滤已有结算单的项目） -->
<template>
  <common-select
    v-model="selectValue"
    :loading="loading"
    :options="options"
    :type="'other'"
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

import useUserProjects from '@compos/store/use-user-projects'
import { allPT } from '@/settings/config'
import { projectNameFormatter } from '@/utils/project'

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
  defaultId: {
    type: [Number, String],
    default: undefined
  },
  projectType: {
    // 项目类型
    type: Number,
    default: allPT // 全部
  },
  placeholder: {
    type: String,
    default: '请选择项目'
  }
})

const loading = ref(false)
const selectValue = ref()

const { userProjectsMap } = useUserProjects()

const options = computed(() => {
  const arr = userProjectsMap.value[allPT]
  arr.map(p => {
    p.fullName = projectNameFormatter(p, null, false)
  })
  return arr
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

// change
function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
  const findVal = options.value.find(v => v.id === val) || {}
  emit('projectChange', findVal)
}
</script>

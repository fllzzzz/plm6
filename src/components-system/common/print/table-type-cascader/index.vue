<!-- 表格模板类型 -->
<template>
  <el-cascader
    v-model="c_value"
    :options="options"
    :props="cascaderProps"
    :filterable="props.filterable"
    :clearable="props.clearable"
    :disabled="props.disabled"
    :show-all-levels="props.showAllLevels"
    :placeholder="props.placeholder"
    @change="handleChange"
  />
</template>
<script setup>
import { ref, watch, computed, defineProps, defineEmits } from 'vue'

import { moduleType, tableType } from '@/utils/print/table/type'

const emit = defineEmits(['update:value', 'change'])

const props = defineProps({
  value: {
    type: [Array, Number, String],
    default: undefined
  },
  checkStrictly: { // 启用该功能后，可让父子节点取消关联，选择任意一级选项。
    type: Boolean,
    default: false
  },
  expandTrigger: { // 次级菜单的展开方式
    type: String,
    default: 'hover'
  },
  filterable: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  emitPath: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  showAllLevels: {
    type: Boolean,
    default: true
  },
  placeholder: {
    type: String,
    default: '请选择表格'
  }
})

const options = ref([])
const c_value = ref(undefined)
const loading = ref(false)

const cascaderProps = computed(() => {
  return { value: 'K', label: 'L', children: 'children', checkStrictly: props.checkStrictly, expandTrigger: props.expandTrigger, emitPath: props.emitPath }
})

watch(
  () => props.value,
  (value) => {
    if (props.emitPath) {
      c_value.value = props.value ? [...value] : []
    } else {
      c_value.value = value
    }
  },
  { immediate: true }
)

fetch()

function handleChange(val) {
  emit('update:value', val)
  emit('change', tableType[val])
}

function fetch() {
  loading.value = true
  const _options = []
  const typeMap = {}
  for (const type in moduleType) {
    const module = moduleType[type]
    for (const key in module.V) {
      const value = module.V[key]
      const _module = {
        K: key,
        L: value,
        children: []
      }
      typeMap[key] = _module
    }
  }
  for (const key in tableType) {
    if (typeMap[tableType[key].M]) {
      typeMap[tableType[key].M].children.push({
        K: key,
        ...tableType[key]
      })
    }
  }
  for (const type in moduleType) {
    const module = moduleType[type]
    const _module = {
      K: type,
      L: module.L,
      children: []
    }
    // 过滤没有模板的模块
    for (const key in module.V) {
      if (typeMap[key]?.children.length) {
        _module.children.push(typeMap[key])
      }
    }
    if (_module.children.length) {
      _options.push(_module)
    }
  }
  options.value = _options
  loading.value = false
}
</script>

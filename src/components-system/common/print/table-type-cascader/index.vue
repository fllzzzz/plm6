<!-- 模块类型 -->
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

import enumOperate, { moduleTypeEnum, tableTypeEnum } from '@/utils/print/table-type'

const emit = defineEmits(['update:currentKey', 'change'])

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
    default: false
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
  emit('change', tableTypeEnum[val])
}

function fetch() {
  loading.value = true
  let options = []
  const _moduleTypeEnum = JSON.parse(JSON.stringify(moduleTypeEnum))
  for (const moduleType in _moduleTypeEnum) {
    // 将模块的value改为负数，使options能使用emitPath：false
    if (!props.emitPath) {
      _moduleTypeEnum[moduleType].V = -_moduleTypeEnum[moduleType].V
    }
    _moduleTypeEnum[moduleType].children = []
  }
  const tableTypeArr = enumOperate.toArr(tableTypeEnum)
  for (const tableType of tableTypeArr) {
    if (_moduleTypeEnum[tableType.T]) {
      _moduleTypeEnum[tableType.T].children.push(tableType)
    }
  }
  options = enumOperate.toArr(_moduleTypeEnum)
  options.value = options
  loading.value = false
}
</script>

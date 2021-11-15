<!-- 部门级联列表 -->
<template>
  <el-cascader
    v-model="copyValue"
    :options="dept"
    :props="cascaderProps"
    :filterable="props.filterable"
    :clearable="props.clearable"
    :show-all-levels="props.showAllLevels"
    :placeholder="props.placeholder"
    @change="handleChange"
  />
</template>

<script setup>
import { defineEmits, defineProps, computed, watch, ref } from 'vue'
import useDept from '@compos/store/use-dept'

const emit = defineEmits(['update:modelValue', 'change'])

const props = defineProps({
  modelValue: {
    type: [Array, Number]
  },
  checkStrictly: {
    // 启用该功能后，可让父子节点取消关联，选择任意一级选项。
    type: Boolean,
    default: false
  },
  expandTrigger: {
    // 次级菜单的展开方式
    type: String,
    default: 'hover'
  },
  emitPath: {
    type: Boolean,
    default: false
  },
  filterable: {
    type: Boolean,
    default: false
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  showAllLevels: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择部门'
  }
})

const copyValue = ref()

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    checkStrictly: props.checkStrictly,
    expandTrigger: props.expandTrigger,
    emitPath: props.emitPath,
    multiple: props.multiple
  }
})

watch(
  () => props.modelValue,
  (value) => {
    if (value instanceof Array) {
      copyValue.value = [...value]
    } else {
      copyValue.value = value
    }
    handleChange(value)
  },
  { immediate: true }
)

const { dept } = useDept()

function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

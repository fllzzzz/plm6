<!-- 部门级联列表 -->
<template>
  <el-cascader
    v-model="copyValue"
    :options="regional"
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
import useRegional from '@compos/store/use-regional'

const emit = defineEmits(['update:modelValue', 'change'])

const props = defineProps({
  modelValue: {
    type: [Array, Number],
    default: undefined
  },
  filterable: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择地区'
  }
})

const copyValue = ref()

const cascaderProps = computed(() => {
  return {
    label: 'name',
    value: 'id',
    children: 'children'
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

const { regional } = useRegional()

function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

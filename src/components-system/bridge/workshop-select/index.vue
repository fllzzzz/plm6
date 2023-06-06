<!-- 车间:下拉选择框 -->
<template>
  <el-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    @change="handleChange"
  >
    <el-option v-for="item in options" :key="item.value" :label="item.label" :value="item.value" />
  </el-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, defineExpose } from 'vue'
import { isNotBlank, isBlank, deepClone } from '@data-type/index'
import useWorkshop from '@compos/store/use-bridge-workshops'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  factoryId: {
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
    default: '请选择车间'
  },
  defaultValue: {
    type: Boolean,
    default: false
  }
})

const selectValue = ref()
const options = ref([])

const { loaded, workshops } = useWorkshop()

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    if (props.default && isBlank(value) && isNotBlank(options.value)) {
      selectValue.value = options.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

watch(
  workshops,
  (list) => {
    dataFormat()
  },
  { immediate: true, deep: true }
)

watch(
  () => props.factoryId,
  (value) => {
    dataFormat()
  },
  { immediate: true }
)

function handleChange(val) {
  if (props.modelValue !== val) {
    emit('update:modelValue', val)
    emit('change', val)
  }
}

function dataFormat() {
  options.value = []
  let _options = []
  try {
    if (isNotBlank(workshops.value)) {
      let list = deepClone(workshops.value)
      if (props.factoryId) list = list.filter((v) => props.factoryId === v.factoryId)
      _options = list.map((o) => {
        return {
          value: o.id,
          label: o.name
        }
      })
    }
  } catch (error) {
    console.log('获取车间列表', error)
  } finally {
    options.value = _options
    if (isNotBlank(options.value) && props.default && !selectValue.value) {
      selectValue.value = options.value[0].value
    }
    const isExit = options.value.some((v) => v.value === selectValue.value)
    if (!isExit && !props.defaultValue) {
      selectValue.value = undefined
    }
    handleChange(selectValue.value)
  }
}
// 获取车间信息
function getOption(val) {
  return workshops.value.find((k) => k.id === val)
}
defineExpose({
  getOption
})
</script>

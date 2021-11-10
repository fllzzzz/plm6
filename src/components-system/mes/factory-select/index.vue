<!-- 工厂:下拉选择框 -->
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
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'
import useFactory from '@compos/store/use-factories'

const emit = defineEmits(['change', 'update:value'])

const props = defineProps({
  value: {
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
    default: '请选择工厂'
  }
})

const selectValue = ref()
const options = ref([])

const { loaded, factories } = useFactory()

watch(
  () => props.value,
  (value) => {
    selectValue.value = value
    if (props.default && isBlank(value) && isNotBlank(options)) {
      selectValue.value = options.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

watch(
  factories,
  (list) => {
    dataFormat(list)
  },
  { immediate: true, deep: true }
)

function handleChange(val) {
  if (props.value !== val) {
    emit('update:value', val)
    emit('change', val)
  }
}

function dataFormat(list) {
  options.value = []
  let _options = []
  try {
    if (isNotBlank(list)) {
      _options = list.map((o) => {
        return {
          value: o.id,
          label: o.name
        }
      })
    }
  } catch (error) {
    console.log('获取工厂', error)
  } finally {
    options.value = _options
    if (isNotBlank(options.value) && props.default && !selectValue.value) {
      selectValue.value = options.value[0].value
    }
    handleChange(selectValue.value)
  }
}

</script>

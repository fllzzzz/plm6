<!-- 表格类型:下拉选择框 -->
<template>
  <div>
    <el-select
      v-model="c_value"
      :size="props.size"
      :disabled="props.disabled"
      :multiple="props.multiple"
      :collapse-tags="props.collapseTags"
      :loading="loading"
      :clearable="props.clearable"
      :filterable="props.filterable"
      :placeholder="props.placeholder"
    >
      <el-option
        v-if="props.showAll"
        :key="-1"
        :label="props.allLabelText"
        :value="undefined"
      />
      <template v-for="item in tableTypeOptions">
        <el-option
          v-if="unshowOptions.indexOf(item[selectProps.key]) === -1"
          :key="item[selectProps.key]"
          :label="item[selectProps.label]"
          :value="item[selectProps.value]"
        />
      </template>
    </el-select>
  </div>
</template>

<script setup>
import { ref, computed, watch, defineProps, defineEmits } from 'vue'

import enumOperate, { tableTypeEnum } from '@/utils/print/table-type'
const tableTypeArr = enumOperate.toArr(tableTypeEnum)

const emit = defineEmits(['update:value', 'change'])

const props = defineProps({
  value: {
    type: [Number, String, Array],
    default: undefined
  },
  options: {
    type: [Array, Object],
    default: () => []
  },
  moduleType: {
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
  filterable: {
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
  showAll: {
    type: Boolean,
    default: false
  },
  default: {
    type: Boolean,
    default: false
  },
  unshowOptions: { // value
    type: Array,
    default: () => []
  },
  placeholder: {
    type: String,
    default: '请选择表格'
  },
  allLabelText: {
    type: String,
    default: '模块下所有表格模板'
  }
})

const loading = ref(false)
const c_value = ref(undefined)
const selectProps = ref({ key: 'K', label: 'L', value: 'K' })

const tableTypeOptions = computed(() => {
  if (props.moduleType) {
    return tableTypeArr.filter(item => {
      return item.T === props.moduleType
    })
  }
  return tableTypeArr
})

watch(
  () => props.value,
  (value) => {
    c_value.value = value
  },
  { immediate: true }
)

watch(
  () => c_value.value,
  (value) => {
    selectChange(value)
  }
)

watch(
  () => tableTypeOptions.value,
  (value) => {
    if (tableTypeOptions.value && tableTypeOptions.value.length) {
      const exist = tableTypeOptions.value.some(t => t.V === c_value.value)
      if (!exist) {
        if (props.default) {
          c_value.value = tableTypeOptions.value[0].V
        } else {
          c_value.value = undefined
        }
      }
    } else {
      c_value.value = undefined
    }
  },
  { immediate: true }
)

function selectChange(val) {
  emit('update:value', val)
  emit('change', val)
}

// eslint-disable-next-line no-unused-vars
function getOptions() {
  return props.options
}
</script>

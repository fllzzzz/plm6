<template>
  <common-select
    v-model="scope.row.basicClass"
    :options="options"
    :showExtra="props.showExtra"
    type="enum"
    :size="props.size"
    :disabled="disabled"
    :multiple="props.multiple"
    :placeholder="props.placeholder"
    :collapse-tags="props.collapseTags"
    :loading="loading"
    :clearable="props.clearable"
    :filterable="props.filterable"
    :textAlign="props.textAlign"
    @change="selectChange"
    @blur="handleBlur"
  />
</template>

<script setup>
import { defineEmits, defineProps, computed, ref, watch } from 'vue'
import { materialClassificationEnum, rawMatClsEnum, manufClsEnum } from '@enum-ms/classification'
import { isBlank } from '@/utils/data-type'

const emit = defineEmits(['change', 'blur', 'update:modelValue'])

const props = defineProps({
  // raw , manuf
  type: {
    type: String
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
  showOptionAll: {
    type: Boolean,
    default: false
  },
  unshowOptions: {
    type: Array,
    default: () => []
  },
  showExtra: {
    type: Boolean,
    default: false
  },
  extraOption: {
    type: Object,
    default: () => {
      return {
        label: '同上',
        value: -1
      }
    }
  },
  placeholder: {
    type: String,
    default: '基础分类'
  },
  textAlign: {
    type: String,
    default: 'left'
  },
  allLabelText: {
    type: String,
    default: '全部'
  },
  allVal: {
    type: [Number, String],
    default: undefined
  },
  disabledVal: {
    type: Array,
    default: () => []
  },
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const loading = ref(false)
const copyValue = ref()

const options = computed(() => {
  if (props.type === 'raw') return rawMatClsEnum.ENUM
  if (props.type === 'manuf') return manufClsEnum.ENUM
  return materialClassificationEnum.ENUM
})

watch(
  () => props.modelValue,
  (value) => { copyValue.value = value },
  { immediate: true }
)

function selectChange(val) {
  if (isBlank(val)) val = undefined
  emit('update:modelValue', val)
  emit('change', val)
}

function handleBlur(event) {
  emit('blur', event)
}
</script>

<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group style="font-size: 15px" class="inline-block" v-model="copyValue" :size="size" :disabled="disabled" @change="selectChange">
    {{ value }}：
    <el-radio-button v-if="showOptionAll" :label="optionAllValue">全部</el-radio-button>
    <template v-for="item in options">
      <el-radio-button v-if="unshowVal.indexOf(item) === -1" :key="item" :label="item" :disabled="disabledVal.indexOf(item) > -1">
        {{ item }}<slot name="suffix" :item="item"></slot>
      </el-radio-button>
    </template>
  </el-radio-group>
</template>

<script setup>
import { defineProps, defineEmits, defineExpose, ref, watchEffect } from 'vue'
import { ElRadioGroup } from 'element-plus'
import { isNotBlank } from '@/utils/data-type'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: [Number, String, Boolean, undefined],
  size: {
    type: String,
    default: 'small'
  },
  options: {
    type: [Object, Array, Number],
    required: true
  },
  default: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  disabledVal: {
    type: Array,
    default: () => []
  },
  unshowVal: {
    type: Array,
    default: () => []
  },
  type: {
    // dict , enum, other
    type: String,
    default: 'other'
  },
  value: {
    // dict , enum, other
    type: String,
    default: ''
  },
  showOptionAll: {
    type: Boolean,
    default: false
  },
  optionAllValue: {
    type: [Number, String, Boolean],
    default: -999999999 // 设置默认值的原因是 label 为 undefined会报错
  },
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const copyValue = ref()

watchEffect(() => {
  copyValue.value = isNotBlank(props.modelValue) ? props.modelValue : -999999999
  setDefault()
})

function selectChange(val) {
  if (val === -999999999) val = undefined
  if (val === props.modelValue) return
  emit('update:modelValue', val)
  emit('change', val)
}

function setDefault() {
  if (props.default && (copyValue.value === -999999999) && isNotBlank(props.options)) {
    for (const i in props.options) {
      copyValue.value = props.options[i]
      selectChange(copyValue.value)
      return
    }
  }
}

defineExpose({
  selectChange
})
</script>

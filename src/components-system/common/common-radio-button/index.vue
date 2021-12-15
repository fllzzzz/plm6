<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group class="inline-block" v-model="copyValue" :size="size" :disabled="disabled" @change="selectChange">
    <el-radio-button v-if="showOptionAll" :label="optionAllValue">全部</el-radio-button>
    <template v-for="item in options">
      <el-radio-button
        v-if="unshowVal.indexOf(item[DS.value]) === -1"
        :key="item[DS.key]"
        :label="item[DS.value]"
        :disabled="disabledVal.indexOf(item[DS.value]) > -1"
      >
        {{ item[DS.label] }}
      </el-radio-button>
    </template>
  </el-radio-group>
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'
import useCommonDataStructureByType from '@compos/use-common-data-structure-by-type'
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
  showOptionAll: {
    type: Boolean,
    default: false
  },
  optionAllValue: {
    type: [Number, String, Boolean],
    default: 0
  },
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const copyValue = ref()

// 数据结构
const DS = useCommonDataStructureByType(props.type, props.dataStructure)

watchEffect(() => {
  copyValue.value = isNotBlank(props.modelValue) ? props.modelValue : 0
  setDefault()
})

function selectChange(val) {
  if (val === 0) val = undefined
  if (val === props.modelValue) return
  emit('update:modelValue', val)
  emit('change', val)
}

function setDefault() {
  if (props.default && !copyValue.value && isNotBlank(props.options)) {
    for (const i in props.options) {
      copyValue.value = props.options[i][DS.value]
      return
    }
  }
}
</script>

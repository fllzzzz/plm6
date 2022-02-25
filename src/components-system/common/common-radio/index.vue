<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group v-model="copyValue" :size="size" :disabled="disabled" @change="selectChange">
    <template v-for="item in options" :key="item[DS.key]">
      <el-radio v-if="!unshowVal.includes(item[DS.value])"  :label="item[DS.value]" :disabled="disabledVal.includes(item[DS.value])">
        {{ item[DS.label] }}
      </el-radio>
    </template>
  </el-radio-group>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import useCommonDataStructureByType from '@compos/use-common-data-structure-by-type'
import { ElRadioGroup } from 'element-plus'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: [Number, String, Boolean],
  type: {
    // dict , enum, other
    type: String,
    default: 'other'
  },
  size: {
    type: String,
    default: 'small'
  },
  options: {
    type: [Object, Array, Number],
    required: true
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
  dataStructure: {
    // 数据结构， type不选择dict与enum的情景下，可使用
    type: Object
  }
})

const copyValue = ref()

// 数据结构
const DS = useCommonDataStructureByType(props.type, props.dataStructure)

watch(
  () => props.modelValue,
  (value) => {
    copyValue.value = value
  },
  { immediate: true }
)

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

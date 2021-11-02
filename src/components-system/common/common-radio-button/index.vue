<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group class="inline-block" v-model="copyValue" :size="size" :disabled="disabled" @change="selectChange">
    <el-radio-button v-if="showOptionAll" :label="undefined">全部</el-radio-button>
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
import { defineProps, defineEmits, watch, ref } from 'vue'
import useCommonDataStructureByType from '@compos/use-common-data-structure-by-type'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: [Number, String, Boolean],
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
  type: {
    // dict , enum, other
    type: String,
    default: 'other'
  },
  showOptionAll: {
    type: Boolean,
    default: false
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
  (value) => { copyValue.value = value },
  { immediate: true }
)

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

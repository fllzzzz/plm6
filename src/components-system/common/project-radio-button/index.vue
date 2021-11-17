<template>
  <el-radio-group v-model="c_value" :size="size" @change="selectChange">
    <el-radio-button :label="undefined"> 全部项目 </el-radio-button>
    <el-radio-button :label="globalProjectId || -1" :disabled="!globalProjectId"> 当前项目 </el-radio-button>
  </el-radio-group>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: Number,
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  // TODO: 注释
  type: {
    type: String,
    default: 'default' // default / all
  }
})
const { globalProjectId } = mapGetters(['globalProjectId'])
const c_value = ref()

watch(
  () => props.modelValue,
  (val) => {
    c_value.value = val ? val.value : undefined
  }
)

watch(
  () => globalProjectId,
  (val) => {
    if (props.type === 'default' || (props.type === 'all' && props.modelValue)) {
      c_value.value = val.value
      selectChange(c_value.value)
    }
  }
)

function init() {
  if (props.type === 'default' && !props.modelValue && globalProjectId) {
    c_value.value = globalProjectId.value
    selectChange(c_value.value)
  } else {
    c_value.value = props.modelValue
  }
}

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

init()
</script>


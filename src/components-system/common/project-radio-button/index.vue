<template>
  <el-radio-group v-model="copyValue" :size="size" @change="selectChange">
    <el-radio-button :label="0"> 全部项目 </el-radio-button>
    <el-radio-button :label="globalProjectId || -1" :disabled="!globalProjectId"> 当前项目 </el-radio-button>
  </el-radio-group>
</template>

<script setup>
import { defineProps, defineEmits, watchEffect, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { isNotBlank } from '@/utils/data-type'
import { ElRadioGroup } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: [Number, undefined],
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  type: {
    type: String,
    default: 'default' // default / all
  }
})
const { globalProjectId } = mapGetters(['globalProjectId'])
const copyValue = ref()
watchEffect(() => {
  copyValue.value = isNotBlank(props.modelValue) ? props.modelValue : 0
})

watch(
  () => globalProjectId.value,
  (newVal) => {
    if (props.type === 'default' && copyValue.value) {
      selectChange(newVal)
    }
  },
  { immediate: true }
)

function selectChange(val) {
  if (val === 0) val = undefined
  emit('update:modelValue', val)
  emit('change', val)
}

function init() {
  if (props.type === 'default' && isNotBlank(globalProjectId.value)) {
    copyValue.value = globalProjectId.value
    selectChange(copyValue.value)
  }
}
init()
</script>

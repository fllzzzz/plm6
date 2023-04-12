<template>
  <el-steps
    :space="props.space"
    :active="curActive"
    :direction="props.direction"
    :process-status="props.processStatus"
    :finish-status="props.finishStatus"
    :align-center="props.alignCenter"
    :simple="props.simple"
    class="common-step"
    @change="handleChange"
  >
    <el-step
      v-for="item in options"
      :key="item.title"
      :title="item.title"
      :description="item.description"
      :icon="item.icon"
      :status="item.status"
    />
  </el-steps>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: Number,
    default: undefined
  },
  options: {
    type: [Array, Object],
    default: () => []
  },
  space: {
    type: [Number, String]
  },
  direction: {
    type: String,
    default: 'horizontal'
  },
  processStatus: {
    type: String,
    default: 'process'
  },
  finishStatus: {
    type: String,
    default: 'finish'
  },
  alignCenter: {
    type: Boolean,
    default: false
  },
  simple: {
    type: Boolean,
    default: false
  }
})

const curActive = ref()

// 监听modelValue变化, 更新curActive
watch(
  () => props.modelValue,
  (val) => {
    curActive.value = val
  }
)

function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
</script>

<style lang="scss" scoped>
.common-step {

  ::v-deep(.el-step__main) {
    position: absolute;
    top: -8px;
    left: 20px;
    background: #fff;
    padding: 0 15px;
  }

  ::v-deep(.el-step.is-horizontal .el-step__line) {
    right: 15px;
  }
}
</style>

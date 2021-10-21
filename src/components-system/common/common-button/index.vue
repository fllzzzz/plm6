<!-- 普通按钮 -->
<template>
  <div class="inline-block">
    <el-tooltip :effect="props.effect" :content="props.content" :placement="props.placement" :disabled="tooltipDisabled">
      <div class="inline-block">
        <el-button v-bind="$attrs" :size="props.size" :type="props.type" :disabled="props.disabled" :loading="loading" @click.stop="handleClick">
          <span @click.stop="handleClick"><slot /></span>
        </el-button>
      </div>
    </el-tooltip>
  </div>
</template>

<script setup>
import { ElButton } from 'element-plus'
import { defineProps, defineEmits, computed } from 'vue'

const emit = defineEmits(['click'])

const props = defineProps({
  showTip: {
    // 显示提示
    type: Boolean,
    default: false
  },
  effect: {
    // 提示样式
    type: String,
    default: 'dark'
  },
  content: {
    // 提示内容
    type: String,
    default: '数据未发生修改，请修改后提交'
  },
  placement: {
    // 提示位置
    type: String,
    default: 'top'
  },
  disabled: {
    // 提交禁用
    type: Boolean,
    default: false
  },
  loading: {
    // loading
    type: Boolean,
    default: false
  },
  size: {
    // 按钮大小
    type: String,
    default: 'small'
  },
  type: {
    // 按钮样式
    type: String,
    default: 'primary'
  }
})

const tooltipDisabled = computed(() => {
  if (props.showTip) {
    return !props.disabled
  } else {
    return true
  }
})

// 处理禁用，点击按钮内的文字仍可触发点击的情况
function handleClick() {
  if (props.disabled) return
  emit('click')
}
</script>

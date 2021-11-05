<!-- 普通按钮, 避免使用el-button 时产生, disabled后仍可通过点击按钮中的文字触发click事件的BUG -->
<template>
  <span class="common-tip-button inline-block">
    <el-tooltip :effect="props.effect" :content="props.content" :placement="props.placement" :disabled="tooltipDisabled">
      <span class="inline-block" style="width: inherit">
        <el-button
          v-if="slotDefault"
          v-bind="$attrs"
          :icon="icon"
          :size="props.size"
          :type="props.type"
          :disabled="props.disabled"
          :loading="loading"
          @click="handleClick"
        >
          <span @click.stop="handleClick"><slot /></span>
        </el-button>
        <el-button
          v-else
          v-bind="$attrs"
          :icon="icon"
          :size="props.size"
          :type="props.type"
          :disabled="props.disabled"
          :loading="loading"
          @click.stop="handleClick"
        />
      </span>
    </el-tooltip>
  </span>
</template>

<script setup>
import { ElButton } from 'element-plus'
import { defineProps, defineEmits, computed, useSlots } from 'vue'

// 判断<slot/>是否有传值,<el-button></el-button>会产生<span></span>,由与element-ui全局样式的影响, 当按钮只有图标时，这种情况会产生一个margin
const slotDefault = !!useSlots().default

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
    type: String
  },
  icon: {
    type: String
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
function handleClick(event) {
  if (props.disabled) return
  emit('click', event)
}
</script>

<style lang="scss" scoped>
.common-tip-button + .common-tip-button{
  margin-left: 6px;
}

.el-tag + .common-tip-button{
  margin-left: 6px;
}

.common-tip-button + .el-tag {
  margin-left: 6px;
}

span + .common-tip-button {
  margin-left: 6px;
}

.common-tip-button + span {
  margin-left: 6px;
}
</style>

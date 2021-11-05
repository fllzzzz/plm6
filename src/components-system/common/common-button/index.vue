<!-- 普通按钮, 避免使用el-button 时产生, disabled后仍可通过点击按钮中的文字触发click事件的BUG -->
<template>
  <el-button
    class="common-button"
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
    class="common-button"
    v-bind="$attrs"
    :icon="icon"
    :size="props.size"
    :type="props.type"
    :disabled="props.disabled"
    :loading="loading"
    @click.stop="handleClick"
  />
</template>

<script setup>
import { ElButton } from 'element-plus'
import { defineProps, defineEmits, useSlots } from 'vue'

// 判断<slot/>是否有传值,<el-button></el-button>会产生<span></span>,由与element-ui全局样式的影响, 当按钮只有图标时，这种情况会产生一个margin
const slotDefault = !!useSlots().default

const emit = defineEmits(['click'])

const props = defineProps({
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

// 处理禁用，点击按钮内的文字仍可触发点击的情况
function handleClick(event) {
  if (props.disabled === true) return
  emit('click', event)
}
</script>


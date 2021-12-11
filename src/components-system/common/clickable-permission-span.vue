<!-- 带权限的点击文字 -->
<!-- 有权限：显示蓝色可点击，点击后有回调 -->
<!-- 无权限：显示原字体颜色，不可点击，点击无回调 -->
<template>
  <span :class="{ 'text-clickable': checkPermission(props.permission) }" @click="handleClick">
    <slot />
  </span>
</template>

<script setup>
import checkPermission from '@/utils/system/check-permission'
import { defineProps, defineEmits } from 'vue'
const emit = defineEmits(['click'])

const props = defineProps({
  permission: {
    type: Array
  }
})

// 处理点击事件
function handleClick() {
  if (!checkPermission(props.permission)) return
  emit('click')
}
</script>

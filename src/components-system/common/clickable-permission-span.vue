<!-- 带权限的点击文字 -->
<!-- 有权限：显示蓝色可点击，点击后有回调 -->
<!-- 无权限：显示原字体颜色，不可点击，点击无回调 -->
<template>
  <span v-if="isNotBlank(text)" v-bind="$attrs" :class="{ 'text-clickable': checkPermission(props.permission) }" @click="handleClick">{{ text }}</span>
  <span v-else v-bind="$attrs" v-empty-text />
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

const emit = defineEmits(['click'])
const props = defineProps({
  permission: {
    type: Array
  },
  text: { // 点击文字
    type: String
  }
})

// 处理点击事件
function handleClick() {
  if (!checkPermission(props.permission)) return
  emit('click')
}
</script>

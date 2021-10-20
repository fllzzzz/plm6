<template>
  <svg-icon :icon-class="props.icon" />
  <span class="system-menu-title">{{ props.title }}</span>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  // 菜单图标
  icon: {
    type: String,
    default: ''
  },
  // 菜单标题
  title: {
    type: String,
    default: ''
  }
})
</script>

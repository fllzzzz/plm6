<template>
  <span class="vertical-label" :style="style">{{ props.name }}</span>
</template>
<script setup>
import { computed, defineProps } from 'vue'
const props = defineProps({
  name: {
    type: String,
    default: ''
  },
  color: {
    type: String,
    default: '#1682e6'
  },
  textColor: {
    type: String,
    default: '#FFFFFF'
  },
  top: {
    type: String,
    default: '-2px'
  },
  left: {
    type: String,
    default: '10px'
  },
  letterSpacing: {
    type: String,
    default: '2px'
  },
  height: {
    type: String,
    default: undefined
  },
  opacity: {
    type: Number,
    default: 1
  }
})

const style = computed(() => {
  return {
    'background-color': props.color,
    color: props.textColor,
    top: props.top,
    left: props.left,
    letterSpacing: props.letterSpacing,
    height: props.height,
    opacity: props.opacity
  }
})
</script>

<style lang="scss" scoped>
.vertical-label {
  background: #1682e6;
  color: white;
  font-weight: 100;
  position: absolute;
  writing-mode: tb-rl;
  right: 0;
  width: 20px;
  padding: 6px 0 4px 0;
  box-sizing: border-box;
  font-size: 11px;
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 10;
  user-select: none;
  // pointer-events: none;
}
</style>

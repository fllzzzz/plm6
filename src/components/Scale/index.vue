<template>
  <div class="scale-container">
    <div class="icon-box" @click="scaleZoom">
      <svg-icon class="icon" icon-class="comp-zoom" />
    </div>
    <div class="icon-box" @click="scaleZoomOut">
      <svg-icon class="icon" icon-class="comp-zoom-out" />
    </div>
    <div class="icon-box" @click="reset">
      <svg-icon class="icon" icon-class="comp-restore-size" />
    </div>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'

import { debounce } from '@/utils'

const intervals = ref(200)

const emit = defineEmits(['update:value', 'zoom', 'zoom-out', 'reset'])
const props = defineProps({
  value: {
    type: Number,
    default: 1
  },
  step: {
    type: Number,
    default: 0.1
  },
  min: {
    type: Number,
    default: 0.5
  },
  max: {
    type: Number,
    default: 2
  },
  intervals: {
    type: Number,
    default: 200
  }
})

watch(
  () => props.intervals,
  (val) => {
    intervals.value = val
  },
  { immediate: true }
)

const scaleZoom = debounce(function () {
  let val = props.value + props.step
  val = val > props.max ? props.max : val
  if (val !== props.value) {
    emit('update:value', +val.toFixed(1))
    emit('zoom', +val.toFixed(1))
  }
}, intervals.value)

const scaleZoomOut = debounce(function () {
  let val = props.value - props.step
  val = val < props.min ? props.min : val
  if (val !== props.value) {
    emit('update:value', +val.toFixed(1))
    emit('zoom-out', +val.toFixed(1))
  }
}, intervals.value)

const reset = debounce(function () {
  if (props.value !== 1) {
    emit('update:value', 1)
    emit('reset')
  }
}, intervals.value)
</script>

<style lang="scss" scoped>
.scale-container {
  user-select: none;
  margin-left: 5px;
}
.icon-box {
  position: relative;
  display: inline-flex;
  justify-content: center;
  align-items: center;
  width: 25px;
  height: 25px;
  cursor: pointer;
  background: rgb(83, 83, 82);
  border-radius: 50%;
  opacity: 0.5;
  margin-right: 5px;
  :hover {
    width: 80%;
    height: 80%;
  }
  &:hover {
    opacity: 1;
  }
}
.icon {
  position: absolute;
  width: 15px;
  height: 15px;
}
</style>

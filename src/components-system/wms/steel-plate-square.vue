<template>
  <div class="steel-plate-square" :style="squareStyle">
    <span class="width-text">{{ widthText }}</span>
    <span class="length-text">{{ lengthText }}</span>
  </div>
</template>

<script setup>
// import { lengthUnit2px } from '@/utils/convert/unit'
import { defineProps, computed } from 'vue'
const props = defineProps({
  length: {
    type: [Number, String]
  },
  width: {
    type: [Number, String]
  },
  borderColor: {
    type: String,
    default: '#909399'
  },
  color: {
    type: String,
    default: '#e6a23c'
  },
  unit: {
    type: String,
    default: 'mm'
  },
  elHeight: {
    type: Number,
    default: 100
  }
})

const widthText = computed(() => {
  let w = props.width
  if (+props.width > +props.length) {
    w = props.length
  }
  return `${w}${props.unit}`
})

const lengthText = computed(() => {
  let l = props.length
  if (+props.width > +props.length) {
    l = props.width
  }
  return `${l}${props.unit}`
})

const squareStyle = computed(() => {
  const { width, length, elHeight, color, borderColor, unit } = props
  if (!+width || !+length || !color || !elHeight || !borderColor || !unit) {
    return { display: 'none' }
  } else {
    let _w = +width
    let _l = +length
    /**
     * 如果宽度大于长度，增长宽互换
     * 钢板的宽度 = el的高度
     * 钢板的长度 = el的宽度
     */
    if (_w > _l) {
      _w = +length
      _l = +width
    }
    const aspectRatio = _l / _w
    const style = {
      color: color,
      height: `${elHeight}px`,
      width: `${Math.floor(aspectRatio * elHeight)}px`,
      border: '1px solid',
      'border-color': borderColor
    }
    return style
  }
})
</script>

<style lang="scss" scoped>
.steel-plate-square {
  position: relative;
  font-size: 12px;
  .width-text {
    display: inline-block;
    height: 100%;
    text-align: center;
    position: absolute;
    writing-mode: tb-rl;
  }

  .length-text {
    display: inline-block;
    width: 100%;
    text-align: center;
    position: absolute;
    bottom: 5px;
    left: 0;
    right: 0;
  }
}
</style>

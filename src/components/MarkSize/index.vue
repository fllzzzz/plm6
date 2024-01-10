<template>
  <div
    class="mark-size"
    :style="props.customStyle + `${props.direction === 'horizontal' ? 'flex-direction: row;' : 'flex-direction: column;'}`"
  >
    <template v-if="showType!=='unshow'">
      <div :class="`line ${props.direction === 'horizontal' ? 'line-horizontal' : 'line-vertical'}`"></div>
      <div :class="`arrow ${props.direction === 'horizontal' ? 'arrow-horizontal arrow-horizontal-left' : 'arrow-vertical arrow-horizontal-top'}`"></div>
      <div>{{ props.sizeInfo }}</div>
      <div :class="`arrow ${props.direction === 'horizontal' ? 'arrow-horizontal arrow-horizontal-right' : 'arrow-vertical arrow-horizontal-bottom'}`"></div>
      <div :class="`line ${props.direction === 'horizontal' ? 'line-horizontal' : 'line-vertical'}`"></div>
    </template>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  sizeInfo: {
    type: String
  },
  // 方向
  direction: {
    type: String,
    default: 'horizontal'
  },
  customStyle: {
    type: String
  },
  showType: {
    type: String,
    default: undefined
  }
})
</script>

<style lang="scss" scoped>
.mark-size {
  display: flex;
  align-items: center;
  justify-content: space-between;
  position: absolute;

  .arrow {
    background-color: #606266;
  }

  .arrow-horizontal {
    height: 1px;
    flex: 1;
    margin: 0 5px;
  }

  .arrow-horizontal-right::after {
    content: '';
    display: block;
    width: 0;
    height: 0;
    border-top: 5px solid transparent;
    border-bottom: 5px solid transparent;
    border-left: 5px solid #606266;
    position: absolute;
    right: 5px;
    top: 50%;
    transform: translateY(-50%);
  }

  .arrow-horizontal-left::after {
    content: '';
    display: block;
    width: 0;
    height: 0;
    border-top: 5px solid transparent;
    border-bottom: 5px solid transparent;
    border-right: 5px solid #606266;
    position: absolute;
    left: 5px;
    top: 50%;
    transform: translateY(-50%);
  }

  .arrow-horizontal-top::after{
    content: '';
    display: block;
    width: 0;
    height: 0;
    border-left: 5px solid transparent;
    border-right: 5px solid transparent;
    border-bottom: 5px solid #606266;
    position: absolute;
    left: 50%;
    top: 5px;
    transform: translateX(-50%);
  }

  .arrow-horizontal-bottom::after{
    content: '';
    display: block;
    width: 0;
    height: 0;
    border-left: 5px solid transparent;
    border-right: 5px solid transparent;
    border-top: 5px solid #606266;
    position: absolute;
    left: 50%;
    bottom: 5px;
    transform: translateX(-50%);
  }

  .arrow-vertical {
    width: 1px;
    flex: 1;
    margin: 5px 0;
  }

  .line {
    background-color: #606266;
  }

  .line-horizontal {
    width: 1px;
    height: 20px;
  }

  .line-vertical {
    width: 20px;
    height: 1px;
  }
}
</style>

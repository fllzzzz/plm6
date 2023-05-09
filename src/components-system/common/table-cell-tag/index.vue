<template>
  <span class="table-cell-tag-container" :style="{ 'margin-left': `${props.offset}px` }">
    <span v-if="show" class="table-cell-tag" :style="tagStyle">
      <el-tooltip :disabled="unshowTooltip" :open-delay="300" effect="light" :content="props.name" :placement="props.placement">
        <span>{{ props.name.substr(0, 4) }}</span>
      </el-tooltip>
    </span>
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { TAG_DEF_COLOR } from '@/settings/config'

const props = defineProps({
  name: {
    type: String,
    default: ''
  },
  type: { // type 与 color 同时存在时，优先type
    type: String
  },
  color: {
    type: String,
    default: TAG_DEF_COLOR
  },
  textColor: {
    type: String,
    default: '#FFFFFF'
  },
  placement: {
    type: String,
    default: 'left'
  },
  show: {
    type: Boolean,
    default: true
  },
  offset: {
    type: Number,
    default: 0
  }
})

// 不同类型的标签背景色
const typeBgColor = {
  // 工厂标签默认颜色
  factory: '#1682e6',

  // 甲供标签默认颜色
  partyA: '#e64242',

  // 调拨标签默认颜色
  transfer: '#e64242',

  // 调整标签默认颜色
  supplement: '#f56c6c',

  // 出库调拨标签颜色
  transferOutbound: '#409eff',

  // 解冻标签颜色
  unfreeze: '#f78989',

  // 已打印标签默认颜色
  printed: '#0f9747'
}

const unshowTooltip = computed(() => {
  return props.name.length <= 4
})

const tagStyle = computed(() => {
  if (props.name) {
    let bgColor = props.color
    if (props.type && typeBgColor[props.type]) {
      bgColor = typeBgColor[props.type]
    }
    return {
      'background-color': bgColor,
      color: props.textColor
    }
  }
  return {}
})
</script>

<style lang="scss" scoped>
.table-cell-tag {
  background: #1682e6;
  transform: rotate(-45deg);
  color: white;
  font-weight: 100;
  position: absolute;
  top: 5px;
  left: -20px;
  right: 0;
  width: 70px;
  height: 20px;
  font-size: 11px;
  display: flex;
  justify-content: center;
  align-items: center;
}
</style>

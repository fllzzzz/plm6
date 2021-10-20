<template>
  <span class="color-board" :class="[border ? 'board-border' : '', shadow ? 'is-' + shadow + '-shadow' : 'is-always-shadow']">
    <span :style="titleStyle">
      <slot v-if="$slots.header" name="header" />
      <template v-else>
        <i v-if="icon && !loading" :class="icon" />
        <span v-else>{{ title }}</span>
      </template>
    </span>
    <span
      :style="{
        'text-align': align
      }"
      style="border: 1px solid #ebeef5"
    >
      <i v-if="loading" class="el-icon-loading" />
      <span v-if="$slots.default && !loading"><slot /></span>
    </span>
  </span>
</template>

<script>
import ColorUtil from '@/utils/color'
export default {
  props: {
    color: {
      type: String,
      default: '#1890ff'
    },
    align: {
      type: String,
      default: 'center'
    },
    title: String,
    icon: String,
    shadow: String, // always / hover / never
    border: Boolean,
    loading: Boolean
  },
  computed: {
    titleColor() {
      return ColorUtil.isLight(this.color) ? '#000000' : '#ffffff'
    },
    titleStyle() {
      return {
        'background-color': this.color,
        color: this.titleColor,
        'text-align': this.align,
        'border-color': ColorUtil.compare(this.color, '#FFFFFF') ? '#EBEEF5' : this.color, // 颜色设置为白色则设置边框夜色为#EBEEF5
        'border-style': 'solid',
        'border-width': '1px',
        'border-bottom-style': 'none'
      }
    }
  }
}
</script>

<style lang="scss" scoped>
.color-board {
  display: inline-flex;
  font-size: 16px;
  flex-direction: column;
  justify-content: flex-start;
  align-items: center;
  min-width: 150px;
  transition: 0.3s;
  border-radius: 4px;
  overflow: hidden;
  // border: 1px solid #ebeef5;
  > span {
    display: inline-flex;
    padding: 10px;
    box-sizing: border-box;
    width: 100%;
    justify-content: center;
    align-items: center;
    min-height: 40px;
    > span {
      width: 100%;
    }
  }
  > span:first-child {
    flex: none;
    display: inline-flex;
    align-items: center;
  }
  > span:nth-child(2) {
    border-bottom-right-radius: 4px;
    border-bottom-left-radius: 4px;
    flex: 1;
    background: #ffffff;
  }
}
.color-board + .color-board {
  margin-left: 10px;
}
.color-board.is-always-shadow,
.color-boardd.is-hover-shadow:focus,
.color-board.is-hover-shadow:hover {
  box-shadow: 0 2px 12px 0 rgba(0, 0, 0, 0.1);
}
.board-border {
  border: 1px solid #ebeef5;
}
</style>

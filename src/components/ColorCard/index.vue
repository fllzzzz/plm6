<template>
  <div class="color-container">
    <span
      v-for="(color,index) in colors"
      :key="index"
      class="color-drawer"
      :class="{'select-able': selectAble, 'active': value !== undefined && value === color.value}"
      @click="handleSelectChange(color)"
    >
      <span class="color" :style="{'background-color':color.color, 'border': colorBorder ? '': 'none'}" />
      <span class="title ellipsis-text">{{ color.title }}</span>
    </span>
  </div>
</template>

<script>
export default {
  props: {
    value: { // 选中的值
      type: null,
      default: undefined
    },
    colors: { // color,title,value
      type: Array,
      default: () => []
    },
    colorBorder: { // 色卡边框
      type: Boolean,
      default: false
    },
    selectAble: { // 可选
      type: Boolean,
      default: false
    },
    multiple: { // TODO:多选
      type: Boolean,
      default: false
    }
  },
  methods: {
    handleSelectChange(color) {
      if (!this.selectAble) {
        return
      }
      if (!this.multiple) {
        if (color.value !== this.value) {
          this.handleEmit(color)
        } else {
          this.handleEmit()
        }
      }
    },
    handleEmit(color) {
      this.$emit('update:value', color ? color.value : undefined)
      this.$emit('change', color)
    }
  }
}
</script>

<style lang="scss" scoped>
.color-container {
    display: flex;
    justify-content: space-between;
    align-items: center;
    user-select: none;
    .active {
      // border: 1px #303133 solid;
      padding: 5px 10px;
      box-sizing: border-box;
      border-radius: 15px;
      background: #46a6ff;
      color:white;
      .color {
        border:none
      }
    }
}
.select-able {
  cursor: pointer;
}
.color-drawer {
    display: inline-flex;
    flex-direction: row;
    justify-content: flex-start;
    align-items: center;
    color:#303133;
    font-size: 14px;
    .color {
        width: 18px;
        height: 18px;
        border-radius: 50%;
        display: inline-block;
        margin-right: 10px;
        border: 1px solid #dcdfe6;
        flex: none;
    }
    .title {
      flex: auto;
      display: inline-block;
    }
}
.color-drawer +.color-drawer {
    margin-left: 10px;
}

</style>

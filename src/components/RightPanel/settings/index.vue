<template>
  <div class="drawer-container">
    <div>
      <h3 class="drawer-title">页面风格设计</h3>

      <!-- <div class="drawer-item">
        <span>主题颜色</span>
        <theme-picker style="float: right;height: 26px;margin: -3px 8px 0 0;" @change="themeChange" />
      </div> -->

      <div class="drawer-item">
        <span>查看标签</span>
        <el-switch v-model="tagsView" class="drawer-switch" />
      </div>

      <div class="drawer-item">
        <span>固定头部</span>
        <el-switch v-model="fixedHeader" class="drawer-switch" />
      </div>

      <div class="drawer-item">
        <span>侧边栏 Logo</span>
        <el-switch v-model="showSidebarLogo" class="drawer-switch" />
      </div>

      <div class="drawer-item">
        <span>灰色背景</span>
        <el-switch v-model="switchBackgroundColor" class="drawer-switch" />
      </div>
    </div>
  </div>
</template>

<script setup>
import { computed } from 'vue'
import { useStore } from 'vuex'
const store = useStore()

const fixedHeader = computed({
  get() {
    return store.getters.fixedHeader
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['fixedHeader', val]]))
  }
})

const tagsView = computed({
  get() {
    return store.getters.tagsView
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['tagsView', val]]))
  }
})

const showSidebarLogo = computed({
  get() {
    return store.getters.showSidebarLogo
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['showSidebarLogo', val]]))
  }
})

const switchBackgroundColor = computed({
  get() {
    return store.getters.switchBackgroundColor
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['switchBackgroundColor', val]]))
  }
})

// TODO:主题功能未实现
// eslint-disable-next-line no-unused-vars
function themeChange(val) {
  console.log(val)
  this.$store.dispatch('settings/changeSetting', new Map([['theme', val]]))
}
</script>

<style lang="scss" scoped>
.drawer-container {
  padding: 24px;
  font-size: 14px;
  line-height: 1.5;
  word-wrap: break-word;

  .drawer-title {
    margin-bottom: 12px;
    color: rgba(0, 0, 0, 0.85);
    font-size: 14px;
    line-height: 22px;
  }

  .drawer-item {
    color: rgba(0, 0, 0, 0.65);
    font-size: 14px;
    padding: 12px 0;
  }

  .drawer-switch {
    float: right;
  }
}
</style>

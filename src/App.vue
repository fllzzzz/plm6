<template>
  <div id="app">
    <el-config-provider :locale="zhCn">
      <router-view></router-view>
    </el-config-provider>
  </div>
</template>

<script setup>
import { watch, nextTick } from 'vue'
import { useRoute } from 'vue-router'
import { useStore } from 'vuex'
import { ElConfigProvider } from 'element-plus'
import zhCn from 'element-plus/lib/locale/lang/zh-cn'

import { allPT } from '@/settings/config'

const store = useStore()
const route = useRoute()

watch(
  () => route.path,
  () => {
    route.meta.projectType = (route.path === '/' || route.path === '/login') ? allPT : (route.meta.ownProductType || route.meta.projectType)
    nextTick(() => {
      store.dispatch('project/setRouteProjectByMeta', route.meta)
    })
  }
)

</script>

<style lang="scss">
@import './styles/index.scss'; // 全局自定义的css样式
.slide-right-enter-active,
.slide-right-leave-active,
.slide-left-enter-active,
.slide-left-leave-active {
  will-change: transform;
  transition: all 1000ms;
  position: absolute;
}
.slide-right-enter {
  opacity: 0;
  // transform: translate3d(-100%, 0, 0);
}
.slide-right-leave-active {
  opacity: 0;
  // transform: translate3d(100%, 0, 0);
}
.slide-left-enter {
  opacity: 0;
  // transform: translate3d(100%, 0, 0);
}
.slide-left-leave-active {
  opacity: 0;
  // transform: translate3d(-100%, 0, 0);
}
</style>

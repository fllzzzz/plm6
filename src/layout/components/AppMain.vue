<template>
  <section class="app-main">
    <template v-if="!showNeedProjectTip">
      <!-- 正常路由显示的模块
        当路由route.component未正确配置路径或加载方式，
        router-view会产生警告 [warn]:Component is missing template or render function
      -->
      <router-view v-slot="{ Component, route }">
        <transition :name="route.meta.transition ? route.meta.transition : 'fade-transform'" mode="out-in">
          <!-- <keep-alive :include="cachedViews"> -->
            <component :is="Component" :key="key" />
          <!-- </keep-alive> -->
        </transition>
      </router-view>
    </template>
    <template v-else>
      <transition name="fade-transform" mode="out-in">
        <div style="margin: 20px">
          <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择项目，当前页面需要选择项目方可查看 </el-tag><br />
          <el-tag type="primary" size="medium">
            可通过网页头部的“项目下拉选择框”选择项目，若选择框中不存在您所在的项目，请联系项目负责人将您拉入项目中
          </el-tag>
        </div>
      </transition>
    </template>
  </section>
</template>

<script setup>
import { computed } from 'vue'
import { useRoute } from 'vue-router'
import { mapGetters } from '@/store/lib'
const route = useRoute()

// 全局projectId, 缓存页面
// eslint-disable-next-line no-unused-vars
const { globalProjectId, cachedViews } = mapGetters(['globalProjectId', 'cachedViews'])

// 路由key值
const key = computed(() => route.path)

// 显示需要项目的提示模块
const showNeedProjectTip = computed(() => {
  if (globalProjectId) {
    return false
  }
  // 从路由的meta中获取是否需要项目的配置（该配置在动态加载路由时处理）
  const needProject = route.meta.needProject
  return typeof needProject === 'boolean' && needProject
})
</script>

<style lang="scss" scoped>
.app-main {
  /* navbar 的高度为50px  */
  min-height: calc(100vh - 50px);
  width: 100%;
  position: relative;
  // overflow: hidden;
}

.fixed-header + .app-main {
  padding-top: 50px;
}

// 在展示tagsView时的最小高度
.hasTagsView {
  .app-main {
    /* 84 = navbar + tags-view = 50 + 34 */
    min-height: calc(100vh - 84px);
  }

  .fixed-header + .app-main {
    padding-top: 84px;
  }
}
</style>

<template>
  <div :class="{ 'has-logo': showSidebarLogo }">
    <logo v-if="showSidebarLogo" :collapse="isCollapse" />
    <el-scrollbar wrap-class="scrollbar-wrapper">
      <el-menu
        mode="vertical"
        :collapse="isCollapse"
        :background-color="variables.menuBg"
        :text-color="variables.menuText"
        :active-text-color="variables.menuActiveText"
        :default-active="activeMenu"
        :unique-opened="false"
        collapse-transition
      >
        <sidebar-item v-for="r in permissionRoutes" :key="r.path" :item="r" :base-path="r.path" />
      </el-menu>
    </el-scrollbar>
  </div>
</template>

<script setup>
import { computed, reactive } from 'vue'
import { mapGetters } from '@/store/lib'
import { useRoute } from 'vue-router'
import { ElMenu } from 'element-plus'
import Logo from './logo.vue'
import SidebarItem from './sidebar-item'
// vite2 暂不支持这种写法
// import variables from '@/styles/variables.scss'

const route = useRoute()

const variables = reactive({
  menuText: '#fff',
  menuActiveText: '#ffd04b',
  menuBg: '#545c64'
})

// 是否显示logo  / 路由 / sidebar配置
const { showSidebarLogo, permissionRoutes, sidebar } = mapGetters(['showSidebarLogo', 'permissionRoutes', 'sidebar'])

// 当前菜单
const activeMenu = computed(() => {
  const { meta, path } = route
  // 在访问A路由时，可以设置B路由被选中。【暂时没有该情景】
  if (meta.activeMenu) {
    return meta.activeMenu
  }
  return path
})

// 是否折叠状态
const isCollapse = computed(() => !sidebar.value.opened)
</script>

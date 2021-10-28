<template>
  <div v-if="showMenu" class="menu-wrapper">
    111
    <template v-if="isOnlyOne">
      <app-link v-if="lastChild.meta" :to="resolvePath(basePath, lastChild.path)">
        <el-menu-item :index="resolvePath(basePath, lastChild.path)" :class="{ 'submenu-title-noDropdown': !props.isNest }">
          <item :icon="lastChild.meta.icon || (props.item.meta && props.item.meta.icon)" :title="lastChild.meta.title" />
        </el-menu-item>
      </app-link>
    </template>
    <template v-else>
      <el-sub-menu v-if="childNeedProject" ref="subMenu" :index="resolvePath(basePath, props.item.path)" popper-append-to-body>
        <template #title>
          <item v-if="props.item.meta" :icon="props.item.meta.icon" :title="props.item.meta.title" />
        </template>
        <sidebar-item
          v-for="child in props.item.children"
          :key="child.path"
          :is-nest="true"
          :item="child"
          :base-path="resolvePath(basePath, child.path)"
          class="nest-menu"
        />
      </el-sub-menu>
    </template>
  </div>
</template>

<script>
export default {
  name: 'SidebarItem'
}
</script>
<script setup>
// TODO: item.routePath 代替 resolvePath(basePath, item.path) toRefs toref
import { computed, defineProps } from 'vue'
import { mapGetters } from '@/store/lib'
import { resolvePath } from '@/utils/resolve-path'
import { ElSubMenu, ElMenuItem } from 'element-plus'
import Item from './item.vue'
import AppLink from './link.vue'
import { isNotBlank } from '@/utils/data-type'

const props = defineProps({
  item: {
    type: Object,
    required: true
  },
  isNest: {
    type: Boolean,
    default: false
  },
  basePath: {
    type: String,
    default: ''
  }
})

const { currentMenu, globalProjectId } = mapGetters(['currentMenu', 'globalProjectId'])

const showMenu = computed(() => {
  // 1.不隐藏
  const hidden = !props.item.hidden
  // 2. 已经选择菜单 && 当前模块和菜单为同一模块
  const sameModule = props.item.meta && currentMenu.value && currentMenu.value.id === props.item.meta.moduleId
  // 3. 若需要project则globalProjectId必须存在
  // TODO: notNeedProject
  const notNeedProject = true || (props.item.meta && (!props.item.meta.needProject || (globalProjectId && props.item.meta.needProject)))
  return hidden && sameModule && notNeedProject
})

// TODO: 需要项目问题暂不明确，后期修改
// 当前路径最后一个菜单节点
const lastChild = computed(() => {
  const current = props.item
  const children = props.item.children || []
  let lastChild
  let needProjectFlag = false
  const showingChildren = children.filter((item) => {
    // TODO: notNeedProject
    const notNeedProject = true || (item.meta && (!item.meta.needProject || (globalProjectId && item.meta.needProject)))
    if (!notNeedProject) {
      needProjectFlag = true
      return false
    }
    if (item.hidden) {
      return false
    }
    return true
  })
  // 当只有一个子路由器时，默认显示子路由器
  if (showingChildren.length === 1) {
    lastChild = showingChildren[0]
  }
  // 如果没有要显示的子路由器，则显示父级
  if (showingChildren.length === 0 && !needProjectFlag) {
    lastChild = { ...current, path: '' }
  }
  return lastChild
})

// 判断是否只有一个需要显示的节点
const isOnlyOne = computed(() => {
  return isNotBlank(lastChild.value) && !props.item.alwaysShow
})

// 子节点需要项目
const childNeedProject = computed(() => {
  if (props.item.children) {
    return props.item.children.some((c) => {
      // TODO: notNeedProject
      return true || (c.meta && (!c.meta.needProject || (globalProjectId && c.meta.needProject)))
    })
  }
  return false
})
</script>

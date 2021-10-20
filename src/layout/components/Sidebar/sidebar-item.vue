<template>
  <div v-if="showMenu" class="menu-wrapper">
    <template v-if="hasOneShowingChild(props.item.children, props.item)">
      <app-link v-if="onlyOneChild.meta" :to="resolvePath(basePath, onlyOneChild.path)">
        <el-menu-item :index="resolvePath(basePath, onlyOneChild.path)" :class="{ 'submenu-title-noDropdown': !props.isNest }">
          <item :icon="onlyOneChild.meta.icon || (props.item.meta && props.item.meta.icon)" :title="onlyOneChild.meta.title" />
        </el-menu-item>
      </app-link>
    </template>
    <template v-else>
      <el-sub-menu v-if="childNeedProject()" ref="subMenu" :index="resolvePath(basePath, props.item.path)" popper-append-to-body>
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
// TODO: item.routePath 代替 resolvePath(basePath, item.path)
import { computed, defineProps, reactive } from 'vue'
import { mapGetters } from '@/store/lib'
import { resolvePath } from '@/utils/resolve-path'
import { ElSubMenu, ElMenuItem } from 'element-plus'
import Item from './item.vue'
import AppLink from './link.vue'

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

let onlyOneChild = reactive({})
function hasOneShowingChild(children = [], parent) {
  let hasOne = false
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
    Object.assign(onlyOneChild, showingChildren[0])
    hasOne = true
  }
  // 如果没有要显示的子路由器，则显示父级
  if (showingChildren.length === 0 && !needProjectFlag) {
    onlyOneChild = Object.assign({ ...parent, path: '', noShowingChildren: true })
    hasOne = true
  }
  if (hasOne) {
    // 该唯一节点不存在子节点或者没有要显示的子节点,TODO:!props.item.alwaysShow
    hasOne = (!onlyOneChild.children || onlyOneChild.noShowingChildren) && !props.item.alwaysShow
  }
  return hasOne
}

function childNeedProject() {
  if (props.item.children) {
    return props.item.children.some((c) => {
      // TODO: notNeedProject
      return true || (c.meta && (!c.meta.needProject || (globalProjectId && c.meta.needProject)))
    })
  }
  return false
}
</script>

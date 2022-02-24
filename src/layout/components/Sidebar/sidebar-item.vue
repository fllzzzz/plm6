<template>
  <div v-if="showMenu" class="menu-wrapper">
    <template v-if="isOnlyOne">
      <app-link v-if="lastChild.meta" :to="resolvePath(basePath, lastChild.path)">
        <template v-if="item.name && allEnclosure.indexOf(item.name)>-1">
          <el-menu-item v-if="showItem.indexOf(item.name)>-1" :index="resolvePath(basePath, lastChild.path)" :class="{ 'submenu-title-noDropdown': !props.isNest }">
            <svg-icon :icon-class="lastChild.meta.icon || (props.item.meta && props.item.meta.icon)" />
            <template #title class="system-menu-title">{{ lastChild.meta.title }}</template>
          </el-menu-item>
        </template>
        <el-menu-item v-else-if="judgeShowing(lastChild.meta)" :index="resolvePath(basePath, lastChild.path)" :class="{ 'submenu-title-noDropdown': !props.isNest }">
          <svg-icon :icon-class="lastChild.meta.icon || (props.item.meta && props.item.meta.icon)" />
          <template #title class="system-menu-title">{{ lastChild.meta.title }}</template>
        </el-menu-item>
      </app-link>
    </template>
    <template v-else>
      <el-sub-menu v-if="childNeedProject && judgeShowing(item.meta)" ref="subMenu" :index="resolvePath(basePath, props.item.path)" popper-append-to-body>
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
import { computed, defineProps, watch, ref } from 'vue'
import { mapGetters } from '@/store/lib'
import { resolvePath } from '@/utils/resolve-path'
import { ElSubMenu, ElMenuItem } from 'element-plus'
import Item from './item.vue'
import AppLink from './link.vue'
import { isNotBlank } from '@/utils/data-type'
import { TechnologyTypeAllEnum, businessTypeEnum, projectModeEnum } from '@enum-ms/contract'

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

const { globalProject, currentMenu, globalProjectId, globalProContentBit } = mapGetters(['globalProject', 'currentMenu', 'globalProjectId', 'globalProContentBit'])
const allEnclosure = ['PlanEnclosureList', 'PlanArtifactTreeList', 'PlanArtifactList', 'PlanMachinePartList', 'PlanAssemblyList']
const enclosureItem = [
  { name: 'PlanTrussSupportList', no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V },
  { name: 'PlanSandwichList', no: TechnologyTypeAllEnum.SANDWICH_BOARD.V },
  { name: 'PlanPressedSupportList', no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V },
  { name: 'PlanPressedColorList', no: TechnologyTypeAllEnum.PROFILED_PLATE.V },
  { name: 'PlanArtifactTreeList', no: TechnologyTypeAllEnum.STRUCTURE.V }
]
const showItem = ref([])
watch(
  () => globalProject.value,
  (val) => {
    if (isNotBlank(val)) {
      const arr = []
      const projectContentData = []
      if (val.projectContentList && val.projectContentList.length > 0) {
        val.projectContentList.forEach(v => {
          if (val.businessType === businessTypeEnum.MACHINING.V) {
            v.alias = v.parentCategory
            projectContentData.push(v)
          } else if (val.businessType === businessTypeEnum.INSTALLATION.V) {
            if (v.childrenList && v.childrenList.length > 0) {
              v.childrenList.forEach(value => {
                value.alias = v.alias
                projectContentData.push(value)
              })
            }
          }
        })
      }
      if (projectContentData.length > 0) {
        enclosureItem.forEach(value => {
          const enclosureVal = projectContentData.find(v => Number(v.no) === value.no)
          if (enclosureVal) {
            arr.push(value.name)
            if (value.no === TechnologyTypeAllEnum.STRUCTURE.V) {
              if (globalProject.value.mode !== projectModeEnum.STRUCTURE.V) {
                arr.push('PlanAssemblyList')
              }
              arr.push('PlanArtifactList', 'PlanMachinePartList')
            }
          }
        })
        if (projectContentData.findIndex((k) => k.alias === 'ENCLOSURE') > -1) {
          arr.push('PlanEnclosureList')
        }
      }
      showItem.value = arr
    } else {
      showItem.value = []
    }
  },
  { deep: true, immediate: true }
)
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

// 根据meta内的 productType 和 projectMode 判断是否显示菜单
function judgeShowing(meta) {
  return (!meta.productType || (meta.productType && meta.productType & globalProContentBit.value)) &&
  (!meta.projectMode || (meta.projectMode && meta.projectMode & globalProject.value?.mode))
}
</script>

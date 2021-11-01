<template>
  <!-- 只有 Firefox 中支持 contextmenu 属性 -->
  <div ref="rootRef" id="tags-view-container" class="tags-view-container">
    <scroll-pane ref="scrollPaneRef" class="tags-view-wrapper">
      <router-link
        v-for="tag in visitedViews"
        :ref="setTagRef"
        :key="tag.path"
        :class="isActive(tag) ? 'active' : ''"
        :to="{ path: tag.path, query: tag.query, fullPath: tag.fullPath }"
        tag="span"
        class="tags-view-item"
        @click.middle="closeSelectedTag(tag)"
        @contextmenu.prevent="openMenu(tag, $event)"
      >
        {{ tag.title }}
        <span v-if="!tag.meta.affix" class="el-icon-close" @click.prevent.stop="closeSelectedTag(tag)" />
      </router-link>
    </scroll-pane>
    <ul v-show="menu.visible" :style="{ left: menu.left + 'px', top: menu.top + 'px' }" class="contextmenu">
      <li @click="refreshSelectedTag(tag.selected)">刷新这个标签页</li>
      <li v-if="!(tag.selected.meta && tag.selected.meta.affix)" @click="closeSelectedTag(tag.selected)">关闭这个标签页</li>
      <li @click="closeOthersTags">关闭其他标签页</li>
      <li @click="closeAllTags(tag.selected)">关闭全部标签页</li>
    </ul>
  </div>
</template>

<script setup>
import ScrollPane from './ScrollPane'

import { onMounted, reactive, ref, watch, nextTick } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'
import { useRoute, useRouter } from 'vue-router'
import path from 'path'

const route = useRoute()
const router = useRouter()

const store = useStore()

// dom
const rootRef = ref()
const scrollPaneRef = ref()
const tagRefs = reactive([])
const setTagRef = el => {
  if (el) {
    tagRefs.push(el)
  }
}

// 右键菜单
const menu = reactive({
  visible: false, // 菜单是否展示
  top: 0, // 定位信息
  left: 0
})

// 标签
const tag = reactive({
  selected: {}
})

let affixTags = []

const { currentMenu, routes, visitedViews } = mapGetters(['currentMenu', 'routes', 'visitedViews'])

watch(route, () => {
  addTags()
  moveToCurrentTag()
})

watch(
  () => menu.visible,
  (value) => {
    if (value) {
      document.body.addEventListener('click', closeMenu, { passive: false })
    } else {
      document.body.removeEventListener('click', closeMenu)
    }
  }
)

onMounted(() => {
  initTags()
  addTags()
})

function isActive(inputRoute) {
  return inputRoute.path === route.path
}

function filterAffixTags(routes, basePath = '/') {
  let tags = []
  routes.forEach((route) => {
    if (route.meta && route.meta.affix) {
      const tagPath = path.resolve(basePath, route.path)
      tags.push({
        fullPath: tagPath,
        path: tagPath,
        name: route.name,
        meta: { ...route.meta }
      })
    }
    if (route.children) {
      const tempTags = filterAffixTags(route.children, route.path)
      if (tempTags.length >= 1) {
        tags = [...tags, ...tempTags]
      }
    }
  })
  return tags
}

// 初始化标签
function initTags() {
  affixTags = filterAffixTags(routes.value)
  for (const tag of affixTags) {
    // 必须拥有标签名称
    if (tag.name) {
      store.dispatch('tagsView/addVisitedView', tag)
    }
  }
}

// 新增标签
function addTags() {
  const { name } = route
  if (name) {
    store.dispatch('tagsView/addView', route)
  }
  return false
}

// 视图移动到当前标签
function moveToCurrentTag() {
  const tags = tagRefs
  nextTick(() => {
    for (const tag of tags) {
      // console.log('tag', tag)
      if (tag.to.path === route.path) {
        // TODO:滚动条问题
        // console.log('scrollPaneRef', scrollPaneRef.value)
        // scrollPaneRef.value.moveToTarget(tag)
        // when query is different then update
        if (tag.to.fullPath !== route.fullPath) {
          store.dispatch('tagsView/updateVisitedView', route)
        }
        break
      }
    }
  })
}

// 刷新当前标签页
function refreshSelectedTag(view) {
  store.dispatch('tagsView/delCachedView', view).then(() => {
    console.log('refresh')
    const { fullPath } = view
    nextTick(() => {
      router.replace({
        path: '/redirect' + fullPath
      })
    })
  })
}

// 关闭当前标签页
function closeSelectedTag(view) {
  store.dispatch('tagsView/delView', view).then(({ visitedViews }) => {
    if (isActive(view)) {
      toLastView(visitedViews, view)
    }
  })
}

// 关闭其他标签页
function closeOthersTags() {
  router.push(tag.selected)
  store.dispatch('tagsView/delOthersViews', tag.selected).then(() => {
    moveToCurrentTag()
  })
}

// 关闭所有标签页
function closeAllTags(view) {
  store.dispatch('tagsView/delAllViews').then(({ visitedViews }) => {
    if (affixTags.some((tag) => tag.path === view.path)) {
      return
    }
    toLastView(visitedViews, view)
  })
}

// 跳转到最后一个页面
function toLastView(visitedViews, view) {
  const latestView = visitedViews.slice(-1)[0]
  if (latestView) {
    router.push(latestView.fullPath)
  } else {
    const path = currentMenu.redirect
    const matchResult = matchedPath(view.matched, path)
    if (!matchResult) {
      router.push({ path })
    } else {
      refreshSelectedTag(view)
    }
  }
}

// 匹配路径
function matchedPath(viewMatched, path) {
  let flag = false
  const matched = [...viewMatched]
  if (!matched || matched.length === 0) {
    return flag
  }
  while (matched.length > 0) {
    const item = matched.shift()
    if (item.path === path) {
      if (item.redirect) {
        path = item.redirect
      } else {
        flag = true
        break
      }
    } else {
      break
    }
  }
  return flag
}

// 打开菜单(右键)
function openMenu(tag, e) {
  const menuMinWidth = 105
  const offsetLeft = rootRef.value.getBoundingClientRect().left // container margin left
  const offsetWidth = rootRef.value.offsetWidth // container width
  const maxLeft = offsetWidth - menuMinWidth // left boundary
  const left = e.clientX - offsetLeft + 15 // 15: margin right

  if (left > maxLeft) {
    menu.left = maxLeft
  } else {
    menu.left = left
  }

  menu.top = e.clientY
  menu.visible = true
  tag.selected = tag
}

// 关闭菜单
function closeMenu() {
  menu.visible = false
}
</script>

<style lang="scss" scoped>
.tags-view-container {
  height: 34px;
  width: 100%;
  background: #fff;
  border-bottom: 1px solid #d8dce5;
  box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.12), 0 0 3px 0 rgba(0, 0, 0, 0.04);
  .tags-view-wrapper {
    .tags-view-item {
      display: inline-block;
      position: relative;
      cursor: pointer;
      height: 26px;
      line-height: 26px;
      border: 1px solid #d8dce5;
      color: #495060;
      background: #fff;
      padding: 0 8px;
      font-size: 12px;
      margin-left: 5px;
      margin-top: 4px;
      &:first-of-type {
        margin-left: 15px;
      }
      &:last-of-type {
        margin-right: 15px;
      }
      &.active {
        background-color: #42b983;
        color: #fff;
        border-color: #42b983;
        &::before {
          content: '';
          background: #fff;
          display: inline-block;
          width: 8px;
          height: 8px;
          border-radius: 50%;
          position: relative;
          margin-right: 2px;
        }
      }
    }
  }
  .contextmenu {
    margin: 0;
    background: #fff;
    z-index: 3000;
    position: absolute;
    list-style-type: none;
    padding: 5px 0;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 400;
    color: #333;
    box-shadow: 2px 2px 3px 0 rgba(0, 0, 0, 0.3);
    li {
      margin: 0;
      padding: 7px 16px;
      cursor: pointer;
      &:hover {
        background: #eee;
      }
    }
  }
}
</style>

<style lang="scss">
//reset element css of el-icon-close
.tags-view-wrapper {
  .tags-view-item {
    .el-icon-close {
      width: 16px;
      height: 16px;
      vertical-align: 2px;
      border-radius: 50%;
      text-align: center;
      transition: all 0.3s cubic-bezier(0.645, 0.045, 0.355, 1);
      transform-origin: 100% 50%;
      &:before {
        transform: scale(0.6);
        display: inline-block;
        vertical-align: -3px;
      }
      &:hover {
        background-color: #b4bccc;
        color: #fff;
      }
    }
  }
}
</style>

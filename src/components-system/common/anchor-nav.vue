<!-- 锚点导航 -->
<template>
  <div ref="containerRef" class="toc-wrapper">
    <ul class="toc-items">
      <li v-for="node in anchorList" :key="node.index" class="toc-item">
        <a class="toc-link" :title="node.label" :class="{ active: node.index === activeAnchor }" @click="handleAnchorClick(node)">
          {{ node.label }}
        </a>
      </li>
    </ul>
    <div ref="markerRef" class="toc-marker" :style="{ top: activeAnchor * 24 + 'px' }" />
  </div>
</template>

<script setup>
import { ref, defineProps, defineExpose, onUnmounted, onMounted } from 'vue'

import { debounce } from '@/utils'

const props = defineProps({
  scrollContainerClass: {
    type: String,
    default: '.anchor-container'
  },
  titleClass: {
    type: String,
    default: '.anchor-title'
  }
})

const markerRef = ref()
const containerRef = ref()
const scrollDOM = ref()
const anchorList = ref([])
const activeAnchor = ref(0)
const isListenerScroll = ref(false)

const getAnchorList = () => {
  activeAnchor.value = 0
  // 清空锚点列表
  anchorList.value = []

  if (isListenerScroll.value) {
    scrollDOM.value.removeEventListener('scroll', handleDebounceScroll)
  }

  // 获取需要滚动实例中的章节 DOM 列表
  scrollDOM.value = document.querySelector(props.scrollContainerClass)

  const titleList = Array.from(scrollDOM.value.querySelectorAll(props.titleClass))
  // console.log('获取需要滚动实例中的章节 DOM 列表 titleList ===', titleList)

  // 遍历章节 DOM 列表，填充锚点列表
  titleList.forEach((item, index) => {
    // console.log('当前遍历的 章节 DOM item ===', item.innerText.replace(/[#\s]/g, ''))
    let label = ''
    if (item.dataset?.anchorLabel) {
      label = item.dataset.anchorLabel
    } else {
      label = item.innerText.replace(/[#\s]/g, '')
    }
    console.log(label)
    anchorList.value.push({
      index, // 章节索引
      label: label || '--', // 章节内容，去除#符号
      titleDOM: item // 章节完整 DOM 信息
    })
  })

  scrollDOM.value.addEventListener('scroll', handleDebounceScroll)
  isListenerScroll.value = true
}

const handleAnchorClick = (node) => {
  activeAnchor.value = node.index
  const _scrollTop = node.titleDOM.offsetTop - anchorList.value[0].titleDOM.offsetTop
  scrollDOM.value.scrollTo({
    top: _scrollTop,
    behavior: 'smooth'
  })
}

/**
 * 根据表单已经滚动的高度，判断激活哪个锚点
 */
const activeFixedAnchor = () => {
  // 这里需要注意一个问题，表单实例的 scrollTop 是相对于编辑页面头部的下方开始的，而标题的 offsetTop 是相对于 微应用容器 计算的，因此要加上 65
  const _scrollTop = scrollDOM.value.scrollTop
  const firstOffsetTop = anchorList.value?.length ? anchorList.value[0].titleDOM.offsetTop : 0

  for (let k = 0; k < anchorList.value.length; k++) {
    const curOffsetST = anchorList.value[k].titleDOM.offsetTop - firstOffsetTop
    let nextOffsetST = 0
    if (k < anchorList.value.length - 1) nextOffsetST = anchorList.value[k + 1].titleDOM.offsetTop - firstOffsetTop
    if (
      // 如果 scrollTop 正好和标题节点的 offsetTop 相等
      _scrollTop === curOffsetST ||
      // 由于需要和下一个标题节点作比较，所以当前标题节点不能是最后一个
      (k < anchorList.value.length - 1 &&
        // scrollTop 介于当前判断的标题节点和下一个标题节点之间
        _scrollTop > curOffsetST &&
        _scrollTop < nextOffsetST)
    ) {
      activeAnchor.value = k
      break
      // 如果是最后一个标题节点，只要 scrollTop 大于节点的 offsetTop 即可
    } else if (k === anchorList.value.length - 1) {
      const lastOffsetST = anchorList.value[k - 1].titleDOM.offsetTop - firstOffsetTop
      if (_scrollTop > lastOffsetST) {
        activeAnchor.value = k
        break
      }
    }
  }
}

/**
 * 处理滚动事件
 */
const handleScroll = (e) => {
  e.stopPropagation()
  // 根据表单已经滚动的高度，判断激活哪个锚点
  activeFixedAnchor()
}

/**
 * 对滚动事件进行防抖处理，节约性能
 */
const handleDebounceScroll = debounce(handleScroll, 200)

onMounted(() => {
  // 从滚动实例中，获取章节列表，并填充锚点列表，并添加滚动监听
  getAnchorList()
})

onUnmounted(() => {
  // 移除滚动监听
  scrollDOM.value.removeEventListener('scroll', handleDebounceScroll)
  isListenerScroll.value = false
})

defineExpose({
  refreshAnchorList: getAnchorList
})
</script>

<style lang="scss" scoped>
.toc-wrapper {
  position: relative;

  .toc-marker {
    position: absolute;
    background-color: #0078fc;
    border-radius: 4px;
    width: 1px;
    height: 15px;
    top: 0;
    left: 0;
    z-index: 0;
    transition: top 0.25s cubic-bezier(0, 1, 0.5, 1), opacity 0.25s, background-color 0.5s;
  }

  .toc-items {
    list-style: none;
    padding: 0 0 0 10px;
    margin: 12px 0 0;
    line-height: 1.2;
    border-left: 1px solid #dcdee2;

    .toc-item {
      margin-top: 10px;
      font-size: 12px;
      color: #333333;
      text-overflow: ellipsis;
      overflow: hidden;
      white-space: nowrap;
      color: inherit;

      .toc-link {
        position: relative;
        color: #333333;
        transition: color 0.5s;
        &.active {
          color: #0078fc;
        }
      }
    }
  }
}
</style>

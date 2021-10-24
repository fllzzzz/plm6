<template>
  <!-- onwheel 在 <div> 元素上滚动鼠标滚轮时执行 JavaScript -->
  <el-scrollbar ref="scrollContainerRef" :vertical="false" class="scroll-container" @wheel.passive.prevent="handleScroll">
    <slot />
  </el-scrollbar>
</template>

<script setup>
// FIXME: 代码错误，需要秀改
import { ElScrollbar } from 'element-plus'
import { ref, computed } from 'vue'
const tagAndTagSpacing = 4 // tag间距

const scrollContainerRef = ref()
// const scrollWrapper = computed(() => scrollContainerRef.value.wrapRef.value)
const scrollWrapper = computed(() => scrollContainerRef.value)

function handleScroll(e) {
  const eventDelta = e.wheelDelta || -e.deltaY * 40
  const $scrollWrapper = scrollWrapper.value
  $scrollWrapper.scrollLeft = $scrollWrapper.scrollLeft + eventDelta / 4
}

// eslint-disable-next-line no-unused-vars
function moveToTarget(currentTag) {
  const $container = scrollContainerRef.value.$el
  const $containerWidth = $container.offsetWidth
  const $scrollWrapper = scrollWrapper.value
  // 获取父组件tags对象，有问题
  const tagList = parent.tagRefs

  let firstTag = null
  let lastTag = null

  // 获取第一个tag和最后一个tag
  if (tagList.length > 0) {
    firstTag = tagList[0]
    lastTag = tagList[tagList.length - 1]
  }

  if (firstTag === currentTag) {
    $scrollWrapper.scrollLeft = 0
  } else if (lastTag === currentTag) {
    $scrollWrapper.scrollLeft = $scrollWrapper.scrollWidth - $containerWidth
  } else {
    // 获取前一个tag和下一个tag
    const currentIndex = tagList.findIndex((item) => item === currentTag)
    const prevTag = tagList[currentIndex - 1]
    const nextTag = tagList[currentIndex + 1]

    // the tag's offsetLeft after of nextTag
    const afterNextTagOffsetLeft = nextTag.$el.offsetLeft + nextTag.$el.offsetWidth + tagAndTagSpacing

    // the tag's offsetLeft before of prevTag
    const beforePrevTagOffsetLeft = prevTag.$el.offsetLeft - tagAndTagSpacing

    if (afterNextTagOffsetLeft > $scrollWrapper.scrollLeft + $containerWidth) {
      $scrollWrapper.scrollLeft = afterNextTagOffsetLeft - $containerWidth
    } else if (beforePrevTagOffsetLeft < $scrollWrapper.scrollLeft) {
      $scrollWrapper.scrollLeft = beforePrevTagOffsetLeft
    }
  }
}
</script>

<style lang="scss" scoped>
.scroll-container {
  white-space: nowrap;
  position: relative;
  overflow: hidden;
  width: 100%;
  ::v-deep(.el-scrollbar__bar) {
    bottom: 0px;
  }
  ::v-deep(.el-scrollbar__wrap) {
    height: 49px;
  }
}
</style>

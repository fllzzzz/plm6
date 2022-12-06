<template>
  <div class="project-header-time-query">
    <div v-if="data.length" class="project-header-time-query-content" ref="timeQueryContentRef">
      <div
        v-if="isScroll"
        style="padding: 0 2px; cursor: pointer; border-right: 1px solid #ebebeb; height: 51px; line-height: 51px"
        @click="handleScroll('left')"
      >
        <el-icon><el-arrow-left /></el-icon>
      </div>
      <div class="time-scroll" ref="timeScrollRef">
        <div class="time-list" ref="timeListRef" :style="scrollStyle">
          <div
            class="time-item"
            :style="judgeIsSelected(item.timeStamp) ? `background: rgb(64 158 255);color: #fff;` : ''"
            v-for="(item, index) in data"
            :key="index"
            @click="handleTagClick(item.timeStamp)"
          >
            <div>{{ item.year }}</div>
            <div>{{ item.month }}月</div>
          </div>
        </div>
      </div>
      <div
        v-if="isScroll"
        style="padding: 0 2px; cursor: pointer; border-left: 1px solid #ebebeb; height: 51px; line-height: 51px"
        @click="handleScroll('right')"
      >
        <el-icon><el-arrow-right /></el-icon>
      </div>
    </div>
    <div v-else>
      <el-tag type="warning" size="medium"> * {{ emptyText }}</el-tag>
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, defineEmits, computed, watch, nextTick } from 'vue'
import { isBlank } from '@data-type/index'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: [Number, String, Array, undefined],
    default: undefined
  },
  data: {
    type: Array,
    default: () => []
  },
  multiple: {
    type: Boolean,
    default: false
  },
  emptyText: {
    type: String,
    default: '暂无数据'
  }
})

const timeQueryContentRef = ref()
const timeScrollRef = ref()
const timeListRef = ref()
const currentPage = ref(0)

const isScroll = computed(() => {
  console.log(timeListRef.value?.clientWidth, timeQueryContentRef.value?.clientWidth)
  if (timeListRef.value && timeQueryContentRef.value && timeListRef.value.clientWidth > timeQueryContentRef.value.clientWidth) {
    return true
  }
  return false
})

// 通过计算属性获取 `timeScrollRef.value?.clientWidth` 只能得到 isScroll 为false时的宽度 因此使用监听 isScroll 获取
const scrollSingleWidth = ref()

watch(
  () => isScroll.value,
  () => {
    nextTick(() => {
      scrollSingleWidth.value = timeScrollRef.value?.clientWidth || 0
    })
  },
  { immediate: true }
)

const copyValue = ref()

watch(
  () => props.modelValue,
  (value) => {
    copyValue.value = value
  },
  { immediate: true }
)

const scrollStyle = computed(() => {
  let _w = currentPage.value * scrollSingleWidth.value
  if (timeListRef.value?.clientWidth && (currentPage.value + 1) * scrollSingleWidth.value > timeListRef.value.clientWidth) {
    _w = timeListRef.value.clientWidth - scrollSingleWidth.value
  }
  return !isScroll.value ? '' : `transform:translateX(-${_w}px)`
})

function handleScroll(direction) {
  let _page = currentPage.value
  if (direction === 'left') {
    _page--
  }
  if (direction === 'right' && (_page + 1) * scrollSingleWidth.value < timeListRef.value.clientWidth) {
    _page++
  }
  currentPage.value = _page >= 0 ? _page : 0
}

function judgeIsSelected(item) {
  if (props.multiple) {
    if (isBlank(copyValue.value) || copyValue.value?.indexOf(item) === -1) {
      return false
    }
  } else {
    if (props.modelValue !== item) {
      return false
    }
  }
  return true
}

function handleTagClick(item) {
  if (props.multiple) {
    if (isBlank(copyValue.value)) copyValue.value = []
    const index = copyValue.value.indexOf(item)
    if (index === -1) {
      copyValue.value.push(item)
    } else {
      copyValue.value.splice(index, 1)
    }
    selectChange(copyValue.value)
  } else {
    if (props.modelValue !== item) {
      selectChange(item)
    }
  }
}

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change')
}
</script>

<style lang="scss" scoped>
.project-header-time-query {
  display: flex;
  align-items: center;

  .project-header-time-query-content {
    display: flex;
    align-items: center;
    overflow: hidden;
    // min-width: 100%;
  }

  .time-scroll,
  .time-list {
    display: flex;
  }

  .time-scroll {
    overflow: hidden;
  }

  .time-list {
    transform: translateX(0px);
    transition: all 0.5s;
    box-sizing: border-box;
    border: 1px solid #ebebeb;
  }

  .time-item {
    cursor: pointer;
    white-space: nowrap;
    text-align: center;
    width: 65px;
    font-size: 14px;
    box-sizing: border-box;

    div {
      padding: 3px 5px;

      &:not(:last-child) {
        border-bottom: 1px solid #ebebeb;
      }
    }
    &:not(:last-child) {
      border-right: 1px solid #ebebeb;
    }
  }

  .angle {
    position: relative;

    &::before {
      content: '';
      display: inline-block;
      width: 12px;
      height: 12px;
      background: linear-gradient(-45deg, transparent, transparent, 50%, #ffd04b 50%, #ffd04b 100%);
      position: absolute;
      left: 0px;
      border-radius: 3px 0px 0px 0px;
      margin-right: 2px;
      top: 0px;
    }
  }
}
</style>

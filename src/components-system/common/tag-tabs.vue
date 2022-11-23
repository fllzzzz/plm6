<template>
  <div class="tag-tabs">
    <div class="tag-tabs-content" ref="tagTabsContentRef">
      <span v-if="isScroll" style="margin-right: 5px; cursor: pointer" @click="handleScroll('left')">
        <el-icon><el-arrow-left /></el-icon>
      </span>
      <div class="tag-scroll" ref="tagScrollRef">
        <div class="tag-list" ref="tagListRef" :style="scrollStyle">
          <span class="tag-item" v-for="(item, index) in data" :key="item[itemKey] || index">
            <el-tag
              :hit="hit"
              :effect="item[itemKey] == modelValue ? 'light' : 'plain'"
              :type="item[itemKey] == modelValue ? selectedTagType : unselectedTagType"
              :disable-transitions="true"
              :size="size"
              :class="{ angle: item.hasAngle }"
              @click="handleTagClick(item)"
            >
              <slot :item="item"></slot>
            </el-tag>
          </span>
        </div>
      </div>
      <span v-if="isScroll" style="margin-left: 5px; cursor: pointer" @click="handleScroll('right')">
        <el-icon><el-arrow-right /></el-icon>
      </span>
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, defineExpose, defineEmits, computed, watch, nextTick } from 'vue'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: [Number, String, undefined],
    default: undefined
  },
  data: {
    type: Array,
    default: () => []
  },
  itemKey: {
    type: String,
    default: 'id'
  },
  itemName: {
    type: String,
    default: 'name'
  },
  size: {
    type: String,
    default: 'medium'
  },
  default: {
    type: Boolean,
    default: false
  },
  unselectable: {
    type: Boolean,
    default: false
  },
  hit: {
    type: Boolean,
    default: true
  },
  unselectedTagType: {
    type: String,
    default: 'info'
  },
  selectedTagType: {
    type: String,
    default: 'success'
  }
})

const tagTabsContentRef = ref()
const tagScrollRef = ref()
const tagListRef = ref()
const currentPage = ref(0)

const isScroll = computed(() => {
  console.log(tagListRef.value?.clientWidth, tagTabsContentRef.value?.clientWidth)
  if (tagListRef.value && tagTabsContentRef.value && tagListRef.value.clientWidth > tagTabsContentRef.value.clientWidth) {
    return true
  }
  return false
})

// 通过计算属性获取 `tagScrollRef.value?.clientWidth` 只能得到 isScroll 为false时的宽度 因此使用监听 isScroll 获取
const scrollSingleWidth = ref()

watch(
  () => isScroll.value,
  () => {
    nextTick(() => {
      scrollSingleWidth.value = tagScrollRef.value?.clientWidth || 0
    })
  },
  { immediate: true }
)

const scrollStyle = computed(() => {
  let _w = currentPage.value * scrollSingleWidth.value
  if (tagListRef.value?.clientWidth && (currentPage.value + 1) * scrollSingleWidth.value > tagListRef.value.clientWidth) {
    _w = tagListRef.value.clientWidth - scrollSingleWidth.value
  }
  return !isScroll.value ? '' : `transform:translateX(-${_w}px)`
})

function handleScroll(direction) {
  let _page = currentPage.value
  if (direction === 'left') {
    _page--
  }
  if (direction === 'right' && (_page + 1) * scrollSingleWidth.value < tagListRef.value.clientWidth) {
    _page++
  }
  currentPage.value = _page >= 0 ? _page : 0
}

function handleTagClick(item) {
  if (props.modelValue !== item[props.itemKey]) {
    selectChange(item[props.itemKey])
  } else {
    if (props.unselectable) {
      selectChange(undefined)
    }
  }
}

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

function getOption(val) {
  if (val) {
    return props.data.find((v) => v[props.itemKey] === val)
  }
  return
}

defineExpose({
  getOption
})
</script>

<style lang="scss" scoped>
.tag-tabs {
  display: flex;
  align-items: center;

  .tag-tabs-content {
    display: flex;
    align-items: center;
    overflow: hidden;
  }

  .tag-scroll,
  .tag-list {
    display: flex;
  }

  .tag-scroll {
    overflow: hidden;
  }

  .tag-list {
    transform: translateX(0px);
    transition: all 0.5s;
  }

  .tag-item {
    cursor: pointer;
    white-space: nowrap;

    &:not(:last-child) {
      margin-right: 10px;
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

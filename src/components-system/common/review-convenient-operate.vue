<template>
  <span class="review-convenient-operate-container">
    <el-checkbox v-model="isConsequent" size="mini" @change="handleConsequentChange">连续审核</el-checkbox>
    <span class="prev-text" @click="handlePrev"> 上一条 </span>
    <span class="next-text" @click="handleNext"> 下一条 </span>
  </span>
</template>

<script setup>
import { isNotBlank } from '@/utils/data-type'
import { defineProps, defineEmits, defineExpose, ref, watchEffect, watch } from 'vue'

const emit = defineEmits(['change', 'prev', 'next', 'blank', 'update:modelValue', 'update:currentIndex', 'update:consequent'])

const props = defineProps({
  modelValue: {
    type: [Number, String]
  },
  currentIndex: {
    type: Number,
    default: 0
  },
  list: {
    type: Array,
    default: () => []
  },
  consequent: {
    type: Boolean,
    default: false
  },
  prev: {
    type: Function
  },
  next: {
    type: Function
  }
})

// 当前值
const currentValue = ref()
// 当前下标
const ci = ref(0)
// 是否连续
const isConsequent = ref(false)

const reviewList = ref([])

watch(
  () => props.modelValue,
  () => {
    if (props.modelValue !== currentValue.value) {
      const index = props.list.findIndex((v) => props.modelValue === v)
      currentValue.value = props.modelValue
      ci.value = index
    }
  },
  { immediate: true }
)

watchEffect(() => {
  reviewList.value = props.list
})

watchEffect(() => {
  if (props.currentIndex) {
    ci.value = props.currentIndex
  }
})

watchEffect(() => {
  isConsequent.value = props.consequent
})

watchEffect(() => getCurrentValue())

// 获取当前值
function getCurrentValue() {
  // && reviewList.value.length > ci.value
  const list = reviewList.value
  if (isNotBlank(ci.value) && isNotBlank(list)) {
    const length = list.length
    // 下标小于长度时，获取当前值
    if (ci.value < length) {
      currentValue.value = list[ci.value]
    } else {
      ci.value = 0
    }
  } else {
    currentValue.value = undefined
    ci.value = 0
    // 返回列表已空
    emit('blank')
  }
  handleIndexChange(ci.value)
  handleCurrentValueChange(currentValue.value)
}

// 当前值变更
function handleCurrentValueChange(val) {
  if (props.modelValue === val) return
  emit('update:modelValue', val)
  emit('change', val)
}

// 当前下标变更
function handleIndexChange(val) {
  if (props.currentIndex === val) return
  emit('update:currentIndex', val)
}

// 连续审核改变
function handleConsequentChange(val) {
  if (val === props.consequent) return
  emit('update:consequent', val)
}

// 上一条
function handlePrev() {
  if (typeof props.prev === 'function') {
    props.prev()
  }
  if (--ci.value < 0) {
    ci.value = reviewList.value.length - 1
  }
  emit('prev', ci.value)
}

// 下一条
function handleNext() {
  if (typeof props.next === 'function') {
    props.next()
  }
  if (++ci.value > reviewList.value.length - 1) {
    ci.value = 0
  }
  emit('next', ci.value)
}

// 移除当前下标的值
function removeCurrent() {
  if (reviewList.value.length > 0) {
    reviewList.value.splice(ci.value, 1)
    console.log('index:', ci.value, reviewList.value, reviewList.value.length)
    // 如果位于最后一条，回到第一条
    if (ci.value >= reviewList.value.length - 1) {
      ci.value = 0
    }
  }
}

defineExpose({
  removeCurrent
})
</script>

<style lang="scss" scoped>
.review-convenient-operate-container {
  display: inline-flex;
  align-items: center;
  // margin-right: 10px;
  font-size: 13px;
  > :nth-child(n) {
    margin-right: 7px;
  }
  > :last-child {
    margin-right: 0;
  }
}
.prev-text,
.next-text {
  cursor: pointer;
  &:hover {
    color: #409eff;
  }
}
</style>

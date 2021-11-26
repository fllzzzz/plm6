<template>
  <div class="material-table-spec-select flex-rss" :style="{ 'max-height': `${props.maxHeight}px` }">
    <div class="left-container" :style="{width: `${leftWidth}`}">
      <div class="filter-input-container">
        <el-input v-model="filterText" size="small" placeholder="可输入编号/名称过滤科目" />
      </div>
      <div class="classify-list">
        <el-card shadow="hover">
          <div class="classify-item">
            <span>1</span>
            <span>2</span>
          </div>
        </el-card>
      </div>
    </div>
    <material-spec-select
      ref="specRef"
      class="right-container spec-select"
      :mode="props.mode"
      :show-classify="false"
      :max-height="props.maxHeight"
      @accumulate-change="handleAccumulateChange"
      @select-change="handleSelectChange"
    />
  </div>
</template>

<script setup>
import { defineEmits, defineProps, defineExpose, ref, computed } from 'vue'

import materialSpecSelect from '@comp-cls/material-spec-select/index.vue'

const emit = defineEmits(['accumulateChange', 'selectionChange'])

const props = defineProps({
  mode: {
    type: String,
    default: 'accumulator' // accumulator
  },
  maxHeight: {
    type: Number,
    default: 600
  },
  width: {
    type: [Number, String],
    default: 350
  }
})

const specRef = ref({})
const filterText = ref()
const leftWidth = computed(() => {
  if (typeof props.width === 'number') {
    return `${props.width}px`
  }
  return props.width
})

/**
 * selector 模式
 */
function handleAccumulateChange(val) {
  emit('accumulateChange', val)
}

/**
 * accumulator 模式
 */
function handleSelectChange(val) {
  emit('selectionChange', val)
}

defineExpose({
  accReduce: specRef.value.accReduce,
  init: specRef.value.init,
  clear: specRef.value.clear
})
</script>

<style lang="scss" scoped>
.material-table-spec-select {

  .left-container {
    margin-right: 30px;
    flex: none;

    .filter-input-container {
      width: 100%;
      margin-bottom: 20px;
    }

    ::v-deep(.el-card__body) {
      padding: 0;
    }

    .classify-item {
      font-size: 13px;
      height: 30px;
      line-height: 30px;
      >span {
        padding: 0 10px;
      }
      > span:nth-child(1) {
        display: inline-block;
        width: 39%;
        border-right: 1px solid rgb(228, 231, 237);
      }
    }
  }

  .right-container {
    flex: auto;
  }
}
</style>

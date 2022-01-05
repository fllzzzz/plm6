<template>
  <div class="content">
    <span v-if="props.showDisplay">
      <el-tooltip content="是否显示" placement="bottom-start" effect="light" :open-delay="1000">
        <el-button :type="props.data.show ? 'info':''" size="small" class="icon-select-btn" @click="handleDisplayChange(data.show)">
          <svg-icon icon-class="display" />
        </el-button>
      </el-tooltip>
    </span>
    <span v-if="props.showAllPage">
      <el-tooltip :content="`是否每页显示(显示的前提下)\n\n不选择则只在第一页（标题/表头）或最后一页（表尾）显示`" placement="bottom-start" effect="light" :open-delay="1000">
        <el-button :type="props.data.allPage ? 'info':''" size="small" class="icon-select-btn" @click="handleAllPageChange(data.allPage)">
          <svg-icon icon-class="all-page" />
        </el-button>
      </el-tooltip>
    </span>
    <span v-if="props.showBold">
      <el-tooltip content="将字体加粗" placement="bottom-start" effect="light" :open-delay="1000">
        <el-button :type="props.data.bold === 'bold' ? 'info':''" size="small" class="icon-select-btn" @click="handleBoldChange(data.bold)">
          <svg-icon icon-class="bold" />
        </el-button>
      </el-tooltip>
    </span>
    <span v-if="props.showAlign">
      <el-tooltip v-for="align in alignArr" :key="`align_${align.value}`" :content="align.tip" placement="bottom-start" effect="light" :open-delay="1000">
        <el-button :type="props.data.align === align.value ? 'info':''" size="small" class="icon-select-btn" @click="handleAlignChange(align.value)">
          <svg-icon :icon-class="align.icon" />
        </el-button>
      </el-tooltip>
    </span>
    <span v-if="props.showVerticleAlign">
      <el-tooltip v-for="verticleAlign in verticleAlignArr" :key="`verticleAlign_${verticleAlign.value}`" :content="verticleAlign.tip" placement="bottom-start" effect="light" :open-delay="1000">
        <el-button :type="props.data.verticleAlign === verticleAlign.value ? 'info':''" size="small" class="icon-select-btn" @click="handleVerticleAlignChange(verticleAlign.value)">
          <svg-icon :icon-class="verticleAlign.icon" />
        </el-button>
      </el-tooltip>
    </span>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'

import { isBlank } from '@data-type/index'
import { alignEnum, verticleAlignEnum } from '@/utils/print/enum'

const props = defineProps({
  data: {
    type: Object,
    required: true
  },
  showAllPage: {
    type: Boolean,
    default: false
  },
  showDisplay: {
    type: Boolean,
    default: true
  },
  showBold: {
    type: Boolean,
    default: true
  },
  showAlign: {
    type: Boolean,
    default: true
  },
  showVerticleAlign: {
    type: Boolean,
    default: true
  }
})

const alignArr = ref([
  { value: alignEnum.LEFT.V, icon: 'align-left', tip: '左对齐' },
  { value: alignEnum.CENTER.V, icon: 'align-center', tip: '居中对齐' },
  { value: alignEnum.RIGHT.V, icon: 'align-right', tip: '右对齐' }
])

const verticleAlignArr = ref([
  { value: verticleAlignEnum.TOP.V, icon: 'verticle-align-top', tip: '顶端对齐' },
  { value: verticleAlignEnum.CENTER.V, icon: 'verticle-align-center', tip: '垂直居中' },
  { value: verticleAlignEnum.BOTTOM.V, icon: 'verticle-align-bottom', tip: '低端对齐' }
])

init()

function init() {
  // 设置默认值
  if (props.showAllPage && isBlank(props.data.allPage)) {
    this.$set(props.data, 'allPage', false)
  }
  if (props.showDisplay && isBlank(props.data.show)) {
    this.$set(props.data, 'show', false)
  }
  if (props.showBold && isBlank(props.data.bold)) {
    this.$set(props.data, 'bold', 'unset')
  }
  if (props.showAlign && isBlank(props.data.align)) {
    this.$set(props.data, 'align', alignEnum.CENTER.V)
  }
  if (props.showVerticleAlign && isBlank(props.data.verticleAlign)) {
    this.$set(props.data, 'verticleAlign', verticleAlignEnum.CENTER.V)
  }
}

function handleAllPageChange(oldVal) {
  this.$set(props.data, 'allPage', !oldVal)
}

function handleDisplayChange(oldVal) {
  this.$set(props.data, 'show', !oldVal)
}

function handleBoldChange(oldVal) {
  this.$set(props.data, 'bold', oldVal === 'bold' ? 'unset' : 'bold')
}

function handleAlignChange(val) {
  this.$set(props.data, 'align', val)
}

function handleVerticleAlignChange(val) {
  this.$set(props.data, 'verticleAlign', val)
}
</script>

<style lang="scss" scoped>
.content {
  >span+span{
    display: inline-block;
    margin-left: 25px;
    position: relative;
    &::before{
      content:'';
      position: absolute;
      background-color: #dcdfe6;
      top: 2px;
      left: -15px;
      width: 1px;
      height: 28px;
    }
  }
}
</style>

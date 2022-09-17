<template>
  <!-- 发运记录 -->
  <common-drawer
    ref="shipDrawerRef"
    title="出库记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="sales-happened-record"
    size="90%"
  >
    <template #titleAfter>
      <el-tag type="success" effect="plain" size="medium">
        <span v-parse-project="{ project: detailInfo.project, onlyShortName: true }" v-empty-text />
      </el-tag>
      <el-tag effect="plain" size="medium">
        <span>累计发运额：</span>
        <span v-thousand="detailInfo.happenedAmount" v-empty-text />
      </el-tag>
      <el-tag effect="plain" size="medium">
        <span>运输车次：</span>
        <span>{{ detailInfo.transportQuantity || 0 }}</span>
      </el-tag>
    </template>
    <template #content>
      <happenedDetail :project-id="detailInfo.project?.id" />
    </template>
  </common-drawer>
</template>

<script setup>
import { defineEmits, defineProps } from 'vue'

import useVisible from '@/composables/use-visible'
import happenedDetail from './happened-detail'

const emit = defineEmits(['success', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })

</script>

<style lang="scss">
.sales-happened-record {
  .el-drawer__body {
    padding: 0;
  }
  .app-container {
    padding-top: 6px;
  }
}
</style>

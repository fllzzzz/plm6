<template>
  <span>
    <template v-for="(item, si) in props.transferReceipt.source" :key="si">
      <span class="project-ware-text" v-if="item && item.project">
        {{ item.project }}
      </span>
      <span class="public-ware-text" v-else>公共库</span>
      <span v-if="item.factory">（{{ item.factory.name }}）</span>
      <span v-if="si !== props.transferReceipt.source.length - 1">&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;</span>
    </template>
    <template v-if="props.transferReceipt.boolBorrowReturnNotSelf">
      <span class="borrow-direction-icon">▶</span>&nbsp;
      <el-tooltip content="实际借用项目" placement="top">
        <span class="project-ware-text">{{ props.transferReceipt.borrowProject }}</span>
      </el-tooltip>
    </template>
  </span>
</template>

<script setup>
import { defineProps } from 'vue'
const props = defineProps({
  transferReceipt: {
    type: Object,
    default: () => {
      return {}
    }
  }
})
</script>

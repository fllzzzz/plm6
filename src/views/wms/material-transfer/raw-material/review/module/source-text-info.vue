<template>
  <span>
    <template v-for="(item, si) in props.transferReceipt.source" :key="si">
      <span
        class="project-ware-text"
        v-if="item && item.project"
        v-parse-project="{ project: item.project, onlyShortName: true }"
        v-empty-text
      />
      <span class="public-ware-text" v-else>公共库</span>
      <span v-if="item.factory">（{{ item.factory.name }}）</span>
      <span v-if="si !== props.transferReceipt.source.length - 1">&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;</span>
    </template>
    <template v-if="props.transferReceipt.boolBorrowReturnNotSelf">
      <span class="borrow-direction-icon">▶</span>
      <el-tooltip content="实际借用项目" placement="top">
        <span
          class="project-ware-text"
          v-parse-project="{ project: props.transferReceipt.borrowProject, onlyShortName: true }"
          v-empty-text
        />
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

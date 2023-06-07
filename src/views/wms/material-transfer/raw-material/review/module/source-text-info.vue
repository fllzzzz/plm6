<template>
  <span>
    {{ receiptInfo.source }}
    <template v-if="props.transferReceipt.boolBorrowReturnNotSelf">
      <span class="borrow-direction-icon">▶</span>&nbsp;
      <el-tooltip content="实际借用项目" placement="top">
        <span class="project-ware-text">{{ props.transferReceipt.borrowProject }}</span>
      </el-tooltip>
    </template>
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'
const props = defineProps({
  transferReceipt: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const receiptInfo = computed(() => {
  const receipt = JSON.parse(JSON.stringify(props.transferReceipt))

  // 调拨来源数据转换
  const source = receipt.source
  let sourceStr = ''
  let sourceStrArr = []
  if (source && source.length > 0) {
    source.forEach((sInfo, sIndex) => {
      let str = ''
      if (sInfo && sInfo.project) {
        str += sInfo.project
      } else {
        str += '公共库'
      }
      if (sInfo.workshop) {
        str += `（${sInfo.workshop.name}）`
      }
      sourceStrArr.push(str)
    })
    sourceStrArr = Array.from(new Set(sourceStrArr))
    sourceStr = sourceStrArr.join('　/　')
  }
  receipt.source = sourceStr
  return receipt
})
</script>

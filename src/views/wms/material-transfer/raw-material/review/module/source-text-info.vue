<template>
  <span>{{ receiptInfo.source }}</span>
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
      if (sInfo.factory) {
        str += `（${sInfo.factory.name}）`
      }
      sourceStrArr.push(str)
    })
    sourceStrArr = Array.from(new Set(sourceStrArr))
    sourceStr = sourceStrArr.join('　/　')
  }
  if (receipt.boolBorrowReturnNotSelf && receipt.borrowProject) {
    sourceStr += '　▶　'
    sourceStr += receipt.borrowProject.shortName
  }
  receipt.source = sourceStr
  return receipt
})
</script>

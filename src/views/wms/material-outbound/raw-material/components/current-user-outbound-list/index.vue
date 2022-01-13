<template>
  <el-badge :value="outboundListNumber" :hidden="outboundListNumber <= 0" style="margin-right: 6px">
    <common-button v-bind="$attrs" icon="el-icon-tickets" size="mini" type="success" @click="openOutboundList">出库清单</common-button>
  </el-badge>
  <list v-model:visible="outboundListVisible" @refresh="handleRefresh" />
</template>

<script setup>
import { getDetailNumberByCurrentUser } from '@/api/wms/material-outbound/raw-material/review'
import { defineEmits, defineExpose, onMounted, ref } from 'vue'

import list from './module/list.vue'

const emit = defineEmits(['refresh'])

// 出库清单中的记录的数量
const outboundListNumber = ref(0)
// 清单显示
const outboundListVisible = ref(false)

onMounted(async () => {
  getOutboundListNumber()
})

// 获取出库清单数量
async function getOutboundListNumber() {
  outboundListNumber.value = await getDetailNumberByCurrentUser()
  return outboundListNumber.value
}

// 校验数量是否发生变化, 若变化刷新物料仓页面
async function checkListNumberChange() {
  const oldVal = outboundListNumber.value
  const newVal = await getOutboundListNumber()
  if (oldVal !== newVal) {
    emit('refresh')
  }
}

// 打开出库清单
function openOutboundList() {
  outboundListVisible.value = true
  checkListNumberChange()
}

function handleRefresh() {
  getOutboundListNumber()
  emit('refresh')
}

defineExpose({
  updateListNumber: getOutboundListNumber
})
</script>

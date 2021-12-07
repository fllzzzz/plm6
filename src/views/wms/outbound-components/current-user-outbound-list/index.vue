<template>
    <el-badge :value="outboundListNumber" :hidden="outboundListNumber <= 0" style="margin-right: 15px">
      <common-button v-bind="$attrs" icon="el-icon-tickets" size="mini" type="success" @click="openOutboundList">出库清单</common-button>
    </el-badge>
    <list v-model:visible="outboundListVisible" />
</template>

<script setup>
import { getDetailNumberByCurrentUser } from '@/api/wms/outbound/raw-mat-application-review'
import { defineExpose, onMounted, ref } from 'vue'

import list from './module/list.vue'

// 出库清单中的记录的数量
const outboundListNumber = ref(0)
// 清单显示
const outboundListVisible = ref(false)

onMounted(async () => {
  getOutboundListNumber()
})

async function getOutboundListNumber() {
  outboundListNumber.value = await getDetailNumberByCurrentUser()
}

// 打开出库清单
function openOutboundList() {
  outboundListVisible.value = true
}

defineExpose({
  updateListNumber: getOutboundListNumber
})
</script>

<template>
  <div class="material-label-print">
    <el-tabs v-model="activeName" class="print-list-tab" @tab-click="handleClick">
      <el-tab-pane label="单据模式" name="receiptMode">
        <!-- TODO: 不使用v-if，tab切换卡：需要好几秒，页面高度渲染错误 -->
        <receipt-mode v-if="activeName === 'receiptMode'" v-bind="$attrs" @printed-success="fetchNotPrintedMaterialNumber" />
      </el-tab-pane>
      <el-tab-pane label="物料模式" name="materialMode">
        <material-mode v-if="activeName === 'materialMode'" v-bind="$attrs" @printed-success="fetchNotPrintedMaterialNumber" />
      </el-tab-pane>
    </el-tabs>
    <el-tag class="not-printed-info" v-if="notPrintedInfo" type="danger" effect="plain">
      <span style="font-weight: bold">{{ notPrintedInfo }}</span>
    </el-tag>
  </div>
</template>

<script setup>
import { getNotPrintedMaterialNumber } from '@/api/wms/material-label-print/index'
import { materialLabelPrintPM as permission } from '@/page-permission/wms'

import { provide, ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import MaterialMode from './material-mode'
import ReceiptMode from './receipt-mode'

provide('permission', permission)
const activeName = ref('receiptMode')
const notPrintedInfo = ref()

fetchNotPrintedMaterialNumber()

// 加载未打印数量
async function fetchNotPrintedMaterialNumber() {
  const { inboundMaterial = 0, outboundMaterial = 0, transferMaterial = 0, returnMaterial = 0 } = await getNotPrintedMaterialNumber()
  // eslint-disable-next-line no-irregular-whitespace
  notPrintedInfo.value = `未打印物料数量　　入库：${inboundMaterial}　　半出：${outboundMaterial}　　调拨：${transferMaterial}　　退库：${returnMaterial}`
}

function handleClick() {}
</script>

<style lang="scss" scoped>
.print-list-tab {
  padding: 0 15px;
}

.material-label-print {
  position: relative;
}

.not-printed-info {
  position: absolute;
  top: 8px;
  right: 15px;
}

::v-deep(.app-container) {
  padding: 0;
}
</style>

<template>
  <div class="app-container">
    <el-tabs v-model="activeName" class="tab-container">
      <el-tab-pane label="付款列表" name="payment">
        <payment class="tab-content" />
      </el-tab-pane>
      <el-tab-pane label="收票列表" name="invoice">
        <invoice class="tab-content" />
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import Payment from './payment'
import Invoice from './invoice'

const activeName = ref('payment')
</script>

<style lang="scss" scoped>
.tab-content {
  padding: 0;
}
.badge-item {
  ::v-deep(.el-badge__content) {
    top: 10px;
    right: -2px;
  }
}

</style>

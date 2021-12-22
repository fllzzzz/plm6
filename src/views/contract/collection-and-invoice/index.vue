<template>
  <div class="app-container">
    <el-tabs v-model="activeName" class="tab-container">
      <el-tab-pane label="收款管理" name="collection">
        <collection class="tab-content" />
      </el-tab-pane>
      <el-tab-pane label="开票管理" name="invoice">
        <!-- <template slot="label">
          <el-badge :value="unprocessedInvoiceQuantity" :hidden="!unprocessedInvoiceQuantity" class="badge-item">
            <span>开票管理</span>
          </el-badge>
        </template> -->
        <invoice class="tab-content" />
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import Collection from './collection'
import Invoice from './invoice'

const activeName = ref('collection')
</script>

<style lang="scss" scoped>
.tab-content {
  padding: 0;
}
.badge-item {
  >>>.el-badge__content {
    top: 10px;
    right: -2px;
  }
}

</style>

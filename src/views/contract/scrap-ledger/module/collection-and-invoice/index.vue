<template>
  <common-drawer
    title="收款开票"
    size="100%"
    v-model="visible"
    :before-close="handleClose"
  >
  <template #content>
      <el-tabs v-model="activeName" class="tab-container">
        <el-tab-pane label="收款列表" name="collection">
          <template #label>
            <!-- :value="currentRow?.unCheckCollectionCount" :hidden="!currentRow?.unCheckCollectionCount" -->
            <el-badge class="badge-item">
              <span>收款列表</span>
            </el-badge>
          </template>
          <!-- :projectId="props.projectId" -->
          <collection class="tab-content"  :visibleValue="modelValue" @success="emit('success')" :currentRow="currentRow"/>
        </el-tab-pane>
        <el-tab-pane label="开票列表" name="invoice">
          <template #label>
            <!-- :value="currentRow?.unCheckInvoiceCount" :hidden="!currentRow?.unCheckInvoiceCount" -->
            <el-badge class="badge-item">
              <span>开票列表</span>
            </el-badge>
          </template>
          <invoice class="tab-content" :visibleValue="modelValue" @success="emit('success')" :currentRow="currentRow"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import useVisible from '@compos/use-visible'
import Collection from './collection'
import Invoice from './invoice'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  tabName: {
    type: String,
    default: 'collection'
  }
})

const emit = defineEmits(['update:modelValue'])

const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const activeName = ref('collection')

watch(
  () => props.tabName,
  (val) => {
    activeName.value = val
  },
  { deep: true, immediate: true }
)

async function fetchData() {}

</script>

<style lang="scss" scoped>
.tab-content {
  padding: 0;
}
.badge-item {
  ::v-deep(.el-badge__content) {
    top: 10px;
    right: 3px;
  }
}

</style>

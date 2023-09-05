<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="收款开票"
    :wrapper-closable="false"
    size="100%"
  >
    <template #titleAfter>
      <el-tag>项目：{{projectNameFormatter(currentRow)}}</el-tag>
    </template>
    <template #content>
      <el-tabs v-model="activeName" class="tab-container">
        <el-tab-pane label="收款列表" name="collection" v-if="checkPermission(permission.collection?.get)">
          <template #label>
            <el-badge :value="currentRow?.unCheckCollectionCount" :hidden="!currentRow?.unCheckCollectionCount" class="badge-item">
              <span>收款列表</span>
            </el-badge>
          </template>
          <collection class="tab-content" :projectId="props.projectId" :visibleValue="modelValue" @success="emit('success')" :currentRow="currentRow"/>
        </el-tab-pane>
        <el-tab-pane label="开票列表" name="invoice" v-if="checkPermission(permission.invoice?.get)">
          <template #label>
            <el-badge :value="currentRow?.unCheckInvoiceCount" :hidden="!currentRow?.unCheckInvoiceCount" class="badge-item">
              <span>开票列表</span>
            </el-badge>
          </template>
          <invoice class="tab-content" :projectId="props.projectId" :visibleValue="modelValue" @success="emit('success')" :currentRow="currentRow"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </common-drawer>
</template>

<script setup>
import { projectNameFormatter } from '@/utils/project'
import { defineProps, defineEmits, watch, ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import useVisible from '@compos/use-visible'
import checkPermission from '@/utils/system/check-permission'
import Collection from './collection'
import Invoice from './invoice'

const activeName = ref('collection')
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  tabName: {
    type: [String],
    default: 'collection'
  },
  permission: {
    type: Object,
    default: () => {}
  }
})
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.tabName,
  (val) => {
    activeName.value = val
  },
  { deep: true, immediate: true }
)
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

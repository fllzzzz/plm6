<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="付款收票"
    :wrapper-closable="false"
    size="95%"
  >
    <template #content>
      <el-tabs v-model="activeName" class="tab-container">
        <el-tab-pane label="付款列表" name="receive">
          <receive class="tab-content" :projectId="props.projectId" :visibleValue="modelValue"/>
        </el-tab-pane>
        <el-tab-pane label="收票列表" name="invoice">
          <invoice class="tab-content" :projectId="props.projectId" :visibleValue="modelValue"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import useVisible from '@compos/use-visible'
import Receive from './receive'
import Invoice from './invoice'

const activeName = ref('receive')
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  tabName: {
    type: [String],
    default: 'receive'
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
    right: -2px;
  }
}

</style>

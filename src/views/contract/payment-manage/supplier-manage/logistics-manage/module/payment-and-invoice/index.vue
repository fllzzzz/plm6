<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="付款收票"
    :wrapper-closable="false"
    size="100%"
  >
    <template #titleAfter>
      <span v-if="currentRow.supplierName">{{`物流公司:${currentRow.supplierName}`}}</span>
    </template>
    <template #content>
      <el-tabs v-model="activeName" class="tab-container">
        <el-tab-pane label="付款列表" name="payment" v-if="checkPermission(permission.payment?.get)">
          <payment class="tab-content" :currentRow="props.currentRow" :propertyType="props.propertyType" :visibleValue="modelValue"/>
        </el-tab-pane>
        <el-tab-pane label="收票列表" name="invoice" v-if="checkPermission(permission.invoice?.get)">
          <invoice class="tab-content" :currentRow="props.currentRow" :propertyType="props.propertyType" :visibleValue="modelValue" @success="emit('success')"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import useVisible from '@compos/use-visible'
import checkPermission from '@/utils/system/check-permission'
import Payment from './payment'
import Invoice from './invoice'

const activeName = ref('payment')
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  tabName: {
    type: [String],
    default: 'payment'
  },
  propertyType: {
    type: [Number, String],
    default: undefined
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
    right: -2px;
  }
}

</style>

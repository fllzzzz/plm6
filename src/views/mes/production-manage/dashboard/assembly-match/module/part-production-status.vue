<template>
  <common-drawer ref="drawerRef" title="零件生产状态" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="50%">
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="producedQuantity" prop="producedQuantity" :show-overflow-tooltip="true" label="已生产" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.producedQuantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      init()
    }
  },
  { immediate: true }
)

function init() {}
</script>

<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="审核记录"
    :wrapper-closable="false"
    size="90%"
  >
    <template #titleAfter>
      <span v-if="currentRow.serialNumber">{{`采购合同编号:${currentRow.serialNumber}`}}</span>
    </template>
    <template #content>
      <list :currentRow="props.currentRow" :propertyType="props.propertyType" :visibleValue="modelValue" @success="emit('success')"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import useVisible from '@compos/use-visible'
import list from './list'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  propertyType: {
    type: [Number, String],
    default: undefined
  }
})
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

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

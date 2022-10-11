<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="生产状态"
    :wrapper-closable="false"
    size="60%"
  >
    <common-table
      ref="tableRef"
      border
      :data="list"
      :max-height="260"
      style="width: 100%"
      class="table-form"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" min-width="100" show-overflow-tooltip />
      <el-table-column key="specification" prop="specification" label="产线" min-width="160" show-overflow-tooltip />
      <el-table-column key="drawingNumber" prop="drawingNumber" label="工序" align="left" show-overflow-tooltip>
        <template v-slot="scope">
          {{ scope.row.drawingNumber || '-' }}
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'

import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  list: {
    type: Array,
    default: () => []
  }
})

const tableRef = ref()

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>


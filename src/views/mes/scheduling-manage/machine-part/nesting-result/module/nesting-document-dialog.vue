<template>
  <common-dialog
    title="套料文档详情"
    customClass="nesting-document-dialog"
    v-model="nestingDialogVisible"
    :close-on-click-modal="false"
    width="1100px"
    :showClose="true"
    :before-close="handleClose"
  >
    <common-table :data="nestingDocumentList" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="cutNumber" :show-overflow-tooltip="true" label="切割指令号" min-width="120" align="center" />
      <el-table-column prop="cutName" :show-overflow-tooltip="true" label="切割方式" width="100" align="center" />
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="零件总量（件/kg）" width="100" align="center">
        <template #default="{ row }">
          <span>{{ row.quantity }} / {{ row.totalNetWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="nestingList.boolNestCutEnum"
        prop="orderNumber"
        :show-overflow-tooltip="true"
        label="套料文档"
        width="100"
        align="center"
      >
        <template #default="{ row }">
          <common-button size="mini" type="primary" @click="showPdf(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
// import { saveNestingTask } from '@/api/mes/scheduling-manage/common'
import { defineEmits, defineProps, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const nestingDocumentList = ref([])
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  nestingList: {
    type: Object,
    default: () => {}
  }
})

const { visible: nestingDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.nesting-document-dialog',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  nestingDialogVisible
)
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>

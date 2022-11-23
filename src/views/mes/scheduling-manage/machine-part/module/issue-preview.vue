<template>
  <common-dialog title="零件任务下发预览" v-model="dialogVisible" width="1000px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="confirmIt">确 认</common-button>
    </template>
    <common-table
      v-loading="tableLoading"
      :data="tableData"
      :span-method="spanMethod"
      :max-height="maxHeight"
      :stripe="false"
      :data-format="dataFormat"
      style="width: 100%"
      @selection-change="selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" class="selection" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="date" :show-overflow-tooltip="true" label="排产日期" min-width="100" align="center" />
      <el-table-column prop="layWayConfigName" :show-overflow-tooltip="true" label="切割方式" min-width="100" align="center" />
      <el-table-column prop="thick" :show-overflow-tooltip="true" label="板厚（mm）" min-width="100" align="center" />
      <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" min-width="100" align="center" />
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="工作量（件/kg）" min-width="90" align="center">
        <template #default="{ row }">
          <span>{{ row.quantity }} | {{ row.totalNetWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="schedulingQuantity" :show-overflow-tooltip="true" label="数控切割" min-width="90" align="center" />
      <el-table-column prop="schedulingQuantity" :show-overflow-tooltip="true" label="车间>产线>生产组" min-width="90" align="center" />
      <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="完成日期" min-width="90" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
</script>

<template>
  <common-dialog
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    fullscreen
    width="1200px"
    :title="`项目详情`"
    :before-close="handleClose"
    :show-close="true"
    :close-on-click-modal="false"
    top="10vh"
  >
    <div class="head-container">
      <mHeader />
    </div>
    <div style="float: left; margin-bottom: 8px">
      <print-table api-key="contractLedger" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
    </div>
    <!--表格渲染-->
    <common-table ref="tableRef" :data="partData" style="width: 100%">
      <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
      <el-table-column prop="name" label="项目" align="center" min-width="100"></el-table-column>
      <el-table-column prop="monomer" label="单体" align="center" min-width="100"></el-table-column>
      <el-table-column prop="area" label="区域" align="center" min-width="100"></el-table-column>
      <el-table-column prop="serialNumber" label="编号" align="center" min-width="100"></el-table-column>
      <el-table-column prop="specification" label="规格" align="center" min-width="100"></el-table-column>
      <el-table-column prop="material" label="材质" align="center" min-width="100"></el-table-column>
      <el-table-column prop="length" label="长度" align="center" min-width="100"></el-table-column>
      <el-table-column prop="weight" label="单重" align="center" min-width="100"></el-table-column>
      <el-table-column prop="schedulingQuantity" label="排产数" align="center" min-width="100"></el-table-column>
      <el-table-column prop="finishQuantity" label="完成数" align="center" min-width="100"></el-table-column>
      <el-table-column prop="finishWeight" label="完成量" align="center" min-width="100"></el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
import { getCutPart } from '@/api/cutting/project-data'
import mHeader from './module/header.vue'

const emit = defineEmits(['update:visible'])
const partData = ref([])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: CutPart })

async function CutPart() {
  partData.value = await getCutPart(props.detailData.taskId)
  if (partData.value === '没有零件') {
    partData.value = []
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>


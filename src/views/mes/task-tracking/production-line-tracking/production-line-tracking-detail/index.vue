<template>
  <common-drawer
    ref="drawerRef"
    :title="`产线：${detailData.workshopInf}>${detailData.productionLine}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1200"
  >
    <template #titleRight>
      <print-table api-key="productionLineList" size="mini" type="warning" class="filter-item" />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :data="partData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
        <el-table-column prop="monomer" label="项目" align="center" min-width="180"></el-table-column>
        <el-table-column prop="monomer" label="单体" align="center"></el-table-column>
        <el-table-column prop="area" label="区域" align="center"></el-table-column>
        <el-table-column prop="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column prop="specification" label="规格" align="center"></el-table-column>
        <el-table-column prop="quantity" label="任务数" align="center"></el-table-column>
        <el-table-column prop="weight" label="单重" align="center"></el-table-column>
        <el-table-column prop="finishQuantity" label="完成数" align="center"></el-table-column>
        <el-table-column prop="status" label="状态" align="center"></el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
import { getCutPart } from '@/api/cutting/project-data'

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
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  CutPart()
}

async function CutPart() {
  partData.value = await getCutPart(props.detailData.taskId)
  if (partData.value === '没有零件') {
    partData.value = []
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>


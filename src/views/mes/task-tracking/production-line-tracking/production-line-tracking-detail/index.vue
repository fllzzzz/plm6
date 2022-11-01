<template>
  <common-drawer
    ref="drawerRef"
    :title="`产线：${detailData.workShopName}>${detailData.name}`"
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
      <common-table ref="tableRef" :data="productionLineData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
        <el-table-column prop="monomer" label="项目" min-width="180">
          <template #default="{ row }">
            <span>{{row.projectNumber}}-{{row.projectName}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="monomerName" key="monomerName" label="单体" align="center"></el-table-column>
        <el-table-column prop="areaName" key="areaName" label="区域" align="center"></el-table-column>
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column prop="specification" key="specification" label="规格" min-width="120" align="center"></el-table-column>
        <el-table-column prop="quantity" key="quantity" label="任务数" align="center"></el-table-column>
        <el-table-column prop="weight" key="weight" label="单重" align="center"></el-table-column>
        <el-table-column prop="completeQuantity" key="completeQuantity" label="完成数" align="center"></el-table-column>
        <el-table-column prop="status" label="状态" align="center"></el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { productionLineDetail } from '@/api/mes/task-tracking/production-line-tracking.js'
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'

const emit = defineEmits(['update:visible'])
const productionLineData = ref([])
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
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: productionLineDetailGet })

async function productionLineDetailGet() {
  try {
    const data = await productionLineDetail({
      productionLineId: props.detailData.id,
      productType: props.detailData.productType
    })
    productionLineData.value = data
  } catch (e) {
    console.log('获取产线跟踪详情失败', e)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>


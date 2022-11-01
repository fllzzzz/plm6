<template>
  <common-dialog
    ref="drawerRef"
    v-model="dialogVisible"
    direction="rtl"
    fullscreen
    :title="`${props.detailData.process}生产明细`"
    :before-close="handleClose"
    :show-close="false"
    :close-on-click-modal="false"
    top="10vh"
  >
    <template #titleRight>
      <div style="display: flex">
        <print-table :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
        <common-button size="mini" style="margin-left: 8px" @click="handleClose">关 闭</common-button>
      </div>
    </template>
    <!--表格渲染-->
    <common-table ref="tableRef" :data="processDetailData" :max-height="500"       show-summary :summary-method="getSummaries" style="width: 100%">
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="monomerName" label="单体" align="center"></el-table-column>
      <el-table-column prop="areaName" label="区域" align="center"></el-table-column>
      <el-table-column prop="serialNumber" label="编号" align="center"></el-table-column>
      <el-table-column prop="specification" label="规格" align="center"></el-table-column>
      <el-table-column prop="material" label="材质" align="center"></el-table-column>
      <el-table-column prop="length" label="长度" align="center"></el-table-column>
      <el-table-column prop="weight" label="单重（kg）" align="center"></el-table-column>
      <el-table-column prop="quantity" label="清单数" align="center"></el-table-column>
      <el-table-column prop="completeQuantity" label="完成数" align="center">
        <template #default="{ row }">
          <el-tag type="primary" style="cursor: pointer" @click="showQuantity(row)">{{ row.completeQuantity }}</el-tag>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
  <detail-drawer v-model:visible="drawerVisible" :team-data="teamData" />
</template>

<script setup>
import { projectDetail } from '@/api/mes/task-tracking/monthly-task-tracking.js'
import { defineProps, defineEmits, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import detailDrawer from './detail-drawer.vue'

const emit = defineEmits(['update:visible'])
// const processDetailData = ref([])
const serialNumber = ref()
const drawerVisible = ref(false)
const teamData = ref({})

const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  detailData: {
    type: Object,
    default: () => {},
  },
})

const processDetailData = [
  {monomerName: '单体1',areaName: '区域1',serialNumber:'7-1GKT-1', specification: 'PL6*45', material: 'Q235B', length: 1250, weight: 1245.00, quantity: 25, completeQuantity: 2 }
]

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

const { maxHeight } = useMaxHeight({
  paginate: true
})

function showHook() {
  console.log(props.detailData)
}

// 点击完成数显示详情
function showQuantity(row) {
  drawerVisible.value = true
  teamData.value = row
} 
// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity']
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>


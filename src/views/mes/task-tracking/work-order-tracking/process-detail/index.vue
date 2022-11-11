<template>
  <div class="app-container">
    <div v-show="!props.processList?.taskOrderId" class="my-code">*点击左侧表格行查看详情</div>
    <div v-show="props.processList?.taskOrderId">
      <common-table
        ref="tableRef"
        :data="processData"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        highlight-current-row
        style="width: 100%; cursor: pointer"
        @row-click="handleRowChange"
      >
        <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="工序">
          <template v-slot="scope">
            <el-icon v-if="scope.row.status === workOrderTypeEnum.DELAY.V" :size="20" style="top: 5px; color: red"><BellFilled /></el-icon>
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="rate" prop="rate" :show-overflow-tooltip="true" label="进度" width="160px">
          <template v-slot="scope">
            <el-progress
              :text-inside="true"
              stroke-linecap="square"
              :stroke-width="22"
              :percentage="((scope.row.completeQuantity / scope.row.quantity) * 100).toFixed(2)"
              status="success"
            />
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="任务（件/kg）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ (scope.row.mete).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="completeQuantity" prop="completeQuantity" :show-overflow-tooltip="true" label="完成（件/kg）">
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}/{{ (scope.row.completeMete).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <production-line-detail v-model:visible="drawerVisible" :orderId="props.processList.taskOrderId" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { process, machineProcess } from '@/api/mes/task-tracking/work-order-tracking.js'
import { componentTypeEnum, workOrderTypeEnum } from '@enum-ms/mes'
import { ref, defineProps, watch, inject } from 'vue'
import { DP } from '@/settings/config'
import { BellFilled } from '@element-plus/icons'
import useMaxHeight from '@compos/use-max-height'
import productionLineDetail from '../production-line-detail/index.vue'

const props = defineProps({
  processList: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const detailData = ref({})
const processData = ref([])
const drawerVisible = ref(false)

const productType = inject('productType')
watch(
  () => props.processList,
  (val) => {
    if (val) {
      if (productType.value === componentTypeEnum.ARTIFACT.V) {
        processGet()
      } else {
        machineProcessGet()
      }
    }
  },
  { deep: true }
)

async function processGet() {
  processData.value = []
  if (!props.processList?.taskOrderId) {
    return
  }
  try {
    const data = await process({
      productType: productType.value,
      taskOrderId: props.processList.taskOrderId
    })
    processData.value = data?.artifactList?.concat(data?.assembleList || [])
  } catch (e) {
    console.log('获取构件部件工序进度', e)
  }
}
async function machineProcessGet() {
  processData.value = []
  if (!props.processList?.taskOrderId) {
    return
  }
  try {
    const data = await machineProcess({
      productType: productType.value,
      taskOrderId: props.processList.taskOrderId
    })
    processData.value = data || []
  } catch (e) {
    console.log('获取零件工序进度', e)
  }
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleRowChange(row) {
  drawerVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

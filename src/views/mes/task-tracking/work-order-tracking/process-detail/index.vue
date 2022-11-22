<template>
  <div class="app-container">
    <div v-show="!props.processList?.taskOrderId" class="my-code">*点击左侧表格行查看详情</div>
    <div v-show="props.processList?.taskOrderId">
      <div class="head-container">
        <workshop-select
          ref="workshopInfRef"
          v-model="workshopId"
          placeholder="请选择车间"
          :factory-id="factoryId"
          style="width: 200px"
          class="filter-item"
          clearable
          @change="handleWorkshopProductionLineChange"
        />
        <production-line-select
          ref="productionLineRef"
          class="filter-item"
          v-model="productionLineId"
          :factory-id="factoryId"
          :workshop-id="workshopId"
          :productType="lineProductType"
          placeholder="请选择生产线"
          style="width: 200px"
          clearable
          @change="handleWorkshopProductionLineChange"
        />
      </div>
      <div style="margin-bottom: 20px">
        <div
          v-if="productType === componentTypeEnum.ARTIFACT.V && assembleProcessData[0]?.productType === componentTypeEnum.ASSEMBLE.V"
          class="head-container"
        >
          <el-tag effect="dark" :type="componentTypeTag[componentTypeEnum.VK[assembleProcessData[0]?.productType]]">
            {{ componentTypeEnum.VL[assembleProcessData[0]?.productType] }}
          </el-tag>
          <el-tag style="margin-left: 8px" effect="plain">
            {{ assembleProcessData[0]?.productionLine?.name }}>{{ assembleProcessData[0]?.group?.name }}
          </el-tag>
          <span style="margin-left: 8px; font-size: 14px">工单号：{{ assembleProcessData[0]?.orderNumber }}</span>
        </div>
        <common-table
          v-if="productType === componentTypeEnum.ARTIFACT.V && assembleProcessData[0]?.productType === componentTypeEnum.ASSEMBLE.V"
          ref="tableRef"
          :data="assembleProcessData"
          :empty-text="'暂无数据'"
          :max-height="maxHeight / 2"
          highlight-current-row
          style="width: 100%; cursor: pointer"
          @row-click="handleRowChange"
        >
          <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="工序">
            <template v-slot="scope">
              <el-icon v-if="scope.row.status === workOrderTypeEnum.DELAY.V" :size="20" style="top: 5px; color: red">
                <BellFilled />
              </el-icon>
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
              <span>{{ scope.row.quantity }}/{{ scope.row.mete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            key="completeQuantity"
            prop="completeQuantity"
            :show-overflow-tooltip="true"
            label="完成（件/kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeQuantity }}/{{ scope.row.completeMete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="margin-bottom: 20px">
        <div class="head-container" v-if="processData[0]?.productType === componentTypeEnum.ARTIFACT.V">
          <el-tag effect="dark" :type="componentTypeTag[componentTypeEnum.VK[processData[0]?.productType]]">
            {{ componentTypeEnum.VL[processData[0]?.productType] }}
          </el-tag>
          <el-tag style="margin-left: 8px" effect="plain">
            {{ processData[0]?.productionLine?.name }}>{{ processData[0]?.group?.name }}
          </el-tag>
          <span style="margin-left: 8px; font-size: 14px">工单号：{{ processData[0]?.orderNumber }}</span>
        </div>
        <common-table
          v-if="
            (productType === componentTypeEnum.ARTIFACT.V && processData[0]?.productType === componentTypeEnum.ARTIFACT.V) ||
            (productType === componentTypeEnum.MACHINE_PART.V && processData[0]?.productType === componentTypeEnum.MACHINE_PART.V)
          "
          ref="tableRef"
          :data="processData"
          :empty-text="'暂无数据'"
          :max-height="maxHeight / 2"
          highlight-current-row
          style="width: 100%; cursor: pointer"
          @row-click="handleRowChange"
        >
          <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="工序">
            <template v-slot="scope">
              <el-icon v-if="scope.row.status === workOrderTypeEnum.DELAY.V" :size="20" style="top: 5px; color: red">
                <BellFilled />
              </el-icon>
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
              <span>{{ scope.row.quantity }}/{{ scope.row.mete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            key="completeQuantity"
            prop="completeQuantity"
            :show-overflow-tooltip="true"
            label="完成（件/kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeQuantity }}/{{ scope.row.completeMete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
    </div>
    <production-line-detail :project-id="processList?.project?.id" v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { process, machineProcess } from '@/api/mes/task-tracking/work-order-tracking.js'
import { componentTypeEnum, workOrderTypeEnum } from '@enum-ms/mes'
import { ref, defineProps, watch, inject, computed } from 'vue'
import { DP } from '@/settings/config'
import { BellFilled } from '@element-plus/icons'
import useMaxHeight from '@compos/use-max-height'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
import productionLineDetail from '../production-line-detail/index.vue'

// 由于mes枚举构件、部件的type值相同，单独定义枚举type值
const componentTypeTag = {
  [componentTypeEnum.ARTIFACT.K]: 'success',
  [componentTypeEnum.ASSEMBLE.K]: 'warning',
  [componentTypeEnum.MACHINE_PART.K]: ''
}

const props = defineProps({
  processList: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const detailData = ref({})
const processData = ref([])
const assembleProcessData = ref([])
const drawerVisible = ref(false)

const workshopId = ref()
const productionLineId = ref()
const factoryId = ref()

const productType = inject('productType')

// 产线过滤
const lineProductType = computed(() => {
  if (productType.value === componentTypeEnum.ARTIFACT.V) {
    return productType.value | componentTypeEnum.ASSEMBLE.V
  }
  return productType.value
})

watch(
  () => props.processList,
  (val) => {
    if (val) {
      if (productType.value === componentTypeEnum.ARTIFACT.V) {
        workshopId.value = undefined
        productionLineId.value = undefined
        processGet()
      } else {
        workshopId.value = undefined
        productionLineId.value = undefined
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
      // productType: productType.value,
      topTaskOrderId: props.processList.taskOrderId,
      workshopId: workshopId.value,
      productionLineId: productionLineId.value
    })
    // processData.value = data?.artifactList?.concat(data?.assembleList || [])
    processData.value = data?.artifactList || []
    assembleProcessData.value = data?.assembleList || []
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
      topTaskOrderId: props.processList.taskOrderId,
      workshopId: workshopId.value,
      productionLineId: productionLineId.value
    })
    processData.value = data || []
  } catch (e) {
    console.log('获取零件工序进度', e)
  }
}

function handleWorkshopProductionLineChange() {
  if (productType.value === componentTypeEnum.ARTIFACT.V) {
    processGet()
  } else {
    machineProcessGet()
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

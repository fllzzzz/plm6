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
      <div :style="`height: ${maxHeight + 40}px; overflow-y: auto;`">
        <div style="margin-bottom: 20px" v-for="item in elementProcessData" :key="item">
          <div
            v-if="productType === bridgeComponentTypeEnum.BOX.V && item[0]?.productType === bridgeComponentTypeEnum.CELL.V"
            class="head-container"
          >
            <el-tag effect="dark" :type="componentTypeTag[bridgeComponentTypeEnum.VK[item[0]?.productType]]">
              {{ bridgeComponentTypeEnum.VL[item[0]?.productType] }}
            </el-tag>
            <el-tag style="margin-left: 8px" effect="plain"> {{ item[0]?.productionLine?.name }}>{{ item[0]?.group?.name }} </el-tag>
            <span style="margin-left: 8px; font-size: 14px">工单号：{{ item[0]?.orderNumber }}</span>
          </div>
          <common-table
            v-if="productType === bridgeComponentTypeEnum.BOX.V && item[0]?.productType === bridgeComponentTypeEnum.CELL.V"
            ref="tableRef"
            :data="item"
            :empty-text="'暂无数据'"
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
        <div style="margin-bottom: 20px" v-for="item in processData" :key="item">
          <div class="head-container" v-if="item[0]?.productType === bridgeComponentTypeEnum.BOX.V">
            <el-tag effect="dark" :type="componentTypeTag[bridgeComponentTypeEnum.VK[item[0]?.productType]]">
              {{ bridgeComponentTypeEnum.VL[item[0]?.productType] }}
            </el-tag>
            <el-tag style="margin-left: 8px" effect="plain"> {{ item[0]?.productionLine?.name }}>{{ item[0]?.group?.name }} </el-tag>
            <span style="margin-left: 8px; font-size: 14px">工单号：{{ item[0]?.orderNumber }}</span>
          </div>
          <common-table
            v-if="
              (productType === bridgeComponentTypeEnum.BOX.V && item[0]?.productType === bridgeComponentTypeEnum.BOX.V) ||
              (productType === bridgeComponentTypeEnum.MACHINE_PART.V && item[0]?.productType === bridgeComponentTypeEnum.MACHINE_PART.V)
            "
            ref="tableRef"
            :data="item"
            :empty-text="'暂无数据'"
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
        <div style="margin-bottom: 20px" v-if="props.processList?.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
          <div class="head-container" v-if="processData[0]?.productionLine?.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
            <el-tag effect="dark" :type="componentTypeTag[bridgeComponentTypeEnum.VK[processData[0]?.productType]]">
              {{ bridgeComponentTypeEnum.VL[processData[0]?.productType] }}
            </el-tag>
            <el-tag style="margin-left: 8px" effect="plain">
              {{ processData[0]?.productionLine?.name }}>{{ processData[0]?.groups?.name }}
            </el-tag>
            <span style="margin-left: 8px; font-size: 14px">工单号：{{ processData[0]?.order?.name }}</span>
          </div>
          <common-table
            ref="tableRef"
            :data="processData"
            :empty-text="'暂无数据'"
            maxHeight="400"
            highlight-current-row
            style="width: 100%; cursor: pointer"
          >
            <el-table-column align="center" key="monomer.name" prop="monomer.name" :show-overflow-tooltip="true" label="单体" />
            <el-table-column align="center" key="area.name" prop="area.name" :show-overflow-tooltip="true" label="区域" />
            <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="名称" />
            <el-table-column align="center" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" />
            <el-table-column
              header-align="center"
              key="specification"
              prop="specification"
              :show-overflow-tooltip="true"
              label="规格"
              width="140px"
            />
            <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" /> />
            <el-table-column align="center" key="completeQuantity" prop="completeQuantity" :show-overflow-tooltip="true" label="完成数" />
            <el-table-column align="center" key="weight" prop="weight" :show-overflow-tooltip="true" label="单重" />
            <el-table-column align="center" key="status" prop="status" :show-overflow-tooltip="true" label="状态">
              <template #default="{ row }">
                <span style="color: red" v-if="row.status === workOrderTypeEnum.DELAY.V">{{ workOrderTypeEnum.VL[row.status] }}</span>
                <span v-else>{{ workOrderTypeEnum.VL[row.status] }}</span>
              </template>
            </el-table-column>
          </common-table>
          <!--分页组件-->
          <el-pagination
            :total="total"
            :current-page="queryPage.pageNumber"
            :page-size="queryPage.pageSize"
            style="margin-top: 8px"
            layout="total, prev, pager, next, sizes"
            @size-change="handleSizeChange"
            @current-change="handleCurrentChange"
          />
        </div>
      </div>
    </div>
    <production-line-detail :project-id="processList?.project?.id" v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { smartLineProcess, process, machineProcess } from '@/api/bridge/bridge-task-tracking/work-order-tracking.js'
import { workOrderTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { ref, defineProps, watch, inject, computed } from 'vue'
import { DP } from '@/settings/config'
import { BellFilled } from '@element-plus/icons'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import workshopSelect from '@/components-system/bridge/workshop-select'
import productionLineSelect from '@comp-bridge/production-line-select'
import productionLineDetail from '../production-line-detail/index.vue'

// 由于mes枚举分段、单元件的type值相同，单独定义枚举type值
const componentTypeTag = {
  [bridgeComponentTypeEnum.BOX.K]: 'success',
  [bridgeComponentTypeEnum.CELL.K]: 'warning',
  [bridgeComponentTypeEnum.MACHINE_PART.K]: ''
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
const elementProcessData = ref([])
const drawerVisible = ref(false)

const workshopId = ref()
const productionLineId = ref()
const factoryId = ref()

const productType = inject('productType')

// 产线过滤
const lineProductType = computed(() => {
  if (productType.value === bridgeComponentTypeEnum.BOX.V) {
    return productType.value | bridgeComponentTypeEnum.CELL.V
  }
  return productType.value
})

watch(
  () => props.processList,
  (val) => {
    if (val) {
      if (productType.value === bridgeComponentTypeEnum.BOX.V) {
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

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: processGet })

// 传统线/智能线 分段
async function processGet() {
  processData.value = []
  if (!props.processList?.taskOrderId) {
    return
  }
  try {
    const data = await process({
      topTaskOrderId: props.processList.taskOrderId,
      workshopId: workshopId.value,
      productionLineId: productionLineId.value
    })
    if (props.processList?.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
      const { content = [], totalElements } = await smartLineProcess({
        topTaskOrderId: props.processList.taskOrderId,
        workshopId: workshopId.value,
        productionLineId: productionLineId.value,
        ...queryPage
      })
      processData.value = content || []
      setTotalPage(totalElements)
    } else {
      processData.value = data?.boxList
    }
    elementProcessData.value = data?.elementList || []
  } catch (e) {
    console.log('获取分段单元件工序进度', e)
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
  if (productType.value === bridgeComponentTypeEnum.BOX.V) {
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

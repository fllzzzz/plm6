<template>
  <div class="detail-container">
    <div style="margin-bottom: 10px" class="head-container">
      <div>
        <el-tag class="filter-item" size="medium" style="margin-right: 3px">{{
          `项目:${props.currentRow.project.serialNumber + '-' + props.currentRow.project.shortName}`
        }}</el-tag>
        <common-radio-button v-model="category" :options="bridgeShipStatisticsTypeEnum.ENUM" :disabledVal="[bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V]" type="enum" class="filter-item" />
        <monomer-select
          ref="monomerSelectRef"
          v-model="query.monomerId"
          :project-id="props.currentRow.projectId"
          :default="false"
          clearable
          class="filter-item"
          @getAreaInfo="getAreaInfo"
        />
        <common-select
          v-model="query.areaId"
          :options="areaInfo"
          type="other"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          size="small"
          clearable
          placeholder="请选择区域"
          class="filter-item"
          style="width: 200px; margin-left: 3px"
        />
        <print-table
          v-show="category === bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V"
          v-permission="permission.print"
          api-key="bridgeAuxMatDetail"
          :params="{
            projectId: props.currentRow?.projectId,
            workshopId: props.workshopId,
            relationType: bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V,
            ...query,
          }"
          size="mini"
          type="warning"
          class="filter-item"
          style="float: right"
        />
      </div>
      <el-descriptions
        v-show="category === bridgeShipStatisticsTypeEnum.BOX.V || category === bridgeShipStatisticsTypeEnum.DIRECT.V"
        v-loading="summaryLoading"
        :data="summaryData"
        direction="vertical"
        :column="8"
        size="large"
        border
        class="project-summary"
      >
        <el-descriptions-item align="center" label="清单总量（吨）">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('INVENTORY')">{{
            props.weightStatus === weightTypeEnum.NET.V
              ? convertUnits(summaryData?.mete || 0, 'kg', 't', 2)
              : convertUnits(summaryData?.grossMete || 0, 'kg', 't', 2)
          }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="任务总量（吨）">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('ASSIGNMENT')" v-if="category === bridgeShipStatisticsTypeEnum.BOX.V">{{
            props.weightStatus === weightTypeEnum.NET.V
              ? convertUnits(summaryData?.schedulingMete || 0, 'kg', 't', 2)
              : convertUnits(summaryData?.schedulingGrossMete || 0, 'kg', 't', 2)
          }}</span>
          <span v-else>-</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="入库量（吨）">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('STORAGE')">{{
            props.weightStatus === weightTypeEnum.NET.V
              ? convertUnits(summaryData?.inBoundMete || 0, 'kg', 't', 2)
              : convertUnits(summaryData?.inBoundGrossMete || 0, 'kg', 't', 2)
          }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="累计发运">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('CUMULATIVE_SHIPMENT')">{{
            props.weightStatus === weightTypeEnum.NET.V
              ? convertUnits(summaryData?.outBoundMete || 0, 'kg', 't', 2)
              : convertUnits(summaryData?.outBoundGrossMete || 0, 'kg', 't', 2)
          }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="本月发运">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('SHIPMENT_MONTH')">
            {{
              props.weightStatus === weightTypeEnum.NET.V
                ? convertUnits(summaryData?.outMounthBoundMete || 0, 'kg', 't', 2)
                : convertUnits(summaryData?.outMounthBoundGrossMete || 0, 'kg', 't', 2)
            }}
          </span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="库存（吨）">
          <span class="tc-primary" style="cursor: pointer" @click="openDetail('IN_STOCK')">{{
            props.weightStatus === weightTypeEnum.NET.V
              ? convertUnits(summaryData?.stockMete || 0, 'kg', 't', 2)
              : convertUnits(summaryData?.stockGrossMete || 0, 'kg', 't', 2)
          }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="累计车次">
          <!-- <span class="tc-primary" style="cursor: pointer" @click="openDetail('ACCUMULATED_NUMBER')">{{
            summaryData.trainNumber || 0
          }}</span> -->
          <span>{{ summaryData.trainNumber || 0 }}</span>
        </el-descriptions-item>
        <el-descriptions-item v-permission="permission.detail" align="center" label="操作">
          <!-- <span class="tc-primary" style="cursor: pointer" @click="openDetail('ACCUMULATED_NUMBER')">{{
            summaryData.trainNumber || 0
          }}</span> -->
          <common-button type="primary" icon="el-icon-view" size="mini" @click.stop="showDetail" />
        </el-descriptions-item>
      </el-descriptions>
      <common-table
        v-show="category === bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V"
        :data="list"
        v-loading="tableLoading"
        :show-empty-symbol="false"
      >
        <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
        <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" :show-overflow-tooltip="true" min-width="100px" />
        <el-table-column key="areaName" prop="areaName" label="区域" align="center" :show-overflow-tooltip="true" min-width="100px" />
        <el-table-column key="name" prop="name" label="名称" align="center" :show-overflow-tooltip="true" min-width="100px" />
        <el-table-column
          key="specification"
          prop="specification"
          label="规格"
          align="center"
          :show-overflow-tooltip="true"
          min-width="120px"
        />
        <el-table-column key="unit" prop="unit" label="单位" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="quantity" prop="quantity" label="清单量" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="shipQuantity" prop="shipQuantity" label="已发运" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="unCargoQuantity" prop="unCargoQuantity" label="未发运" align="center" :show-overflow-tooltip="true" />
      </common-table>
    </div>
    <component
      :is="showComponent"
      :showType="showType"
      v-model="detailVisible"
      :query="query"
      :category="category"
      :workshopId="props.workshopId"
      :projectId="props.currentRow.projectId"
      :weightStatus="props.weightStatus"
    />
    <detail-drawer
      v-model:visible="drawerVisible"
      :query="query"
      :workshopId="props.workshopId"
      :projectId="props.currentRow.projectId"
      :detail-data="props.currentRow"
      :category="category"
    />
  </div>
</template>

<script setup>
import { ref, defineProps, watch, nextTick, computed } from 'vue'
import { projectSummary, projectSummaryDirect, auxInboundDetail } from '@/api/ship-manage/pack-and-ship/bridge-ship-summary'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'
import { bridgeShipStatisticsTypeEnum } from '@enum-ms/ship-manage'
import monomerSelect from '@/components-system/plan/monomer-select'
import mDetail from './detail.vue'
import directDetail from './direct-detail'
import detailDrawer from './detail-drawer.vue'

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  productionLineTypeEnum: {
    type: Number
  },
  workshopId: {
    type: Number
  },
  weightStatus: {
    type: Number
  }
})

const category = ref(bridgeShipStatisticsTypeEnum.BOX.V)
const list = ref([])
const tableLoading = ref(false)
const areaInfo = ref([])
const query = ref({
  monomerId: undefined,
  areaId: undefined
})
const summaryLoading = ref(false)
const summaryData = ref({})
const showType = ref()
const detailVisible = ref(false)
const drawerVisible = ref(false)

const showComponent = computed(() => {
  return category.value === bridgeShipStatisticsTypeEnum.BOX.V ? mDetail : directDetail
})

watch(
  () => props.currentRow.projectId,
  (val) => {
    if (val) {
      showType.value = undefined
      fetchSummary()
      fetchAuxMat()
    }
  }
)

watch(
  () => query.value,
  (val) => {
    showType.value = undefined
    fetchSummary()
  },
  { immediate: true, deep: true }
)

watch(
  () => category.value,
  (val) => {
    if (val === bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V) {
      fetchAuxMat()
    } else {
      fetchSummary()
    }
  }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}

// 汇总列表
async function fetchSummary() {
  summaryData.value = {}
  summaryLoading.value = true
  if (!props.currentRow?.projectId || !props.permission?.detail) {
    return
  }
  try {
    const api = category.value === bridgeShipStatisticsTypeEnum.BOX.V ? projectSummary : projectSummaryDirect
    const params = {
      projectId: props.currentRow.projectId,
      ...query.value,
      workshopId: props.workshopId
    }
    const data = await api(params)
    summaryData.value = data || {}
  } catch (err) {
    console.log('获取项目发运数据汇总', err)
  } finally {
    summaryLoading.value = false
  }
}

async function fetchAuxMat() {
  try {
    const { content } = await auxInboundDetail({
      projectId: props.currentRow.projectId,
      workshopId: props.workshopId,
      relationType: bridgeShipStatisticsTypeEnum.AUXILIARY_MATERIAL.V,
      ...query.value
    })
    list.value = content || []
  } catch (e) {
    console.log('获取配套件详情失败')
  }
}

function openDetail(show) {
  showType.value = show
  // currentRow.value = row.sourceRow
  // detailQuery.value = {
  //   projectId: row.sourceRow.project.id,
  //   dateTime: crud.query.dateTime
  // }
  nextTick(() => {
    detailVisible.value = true
  })
}

function showDetail() {
  drawerVisible.value = true
}
</script>

<template>
  <div class="app-container project-dashboard">
    <monomer-select ref="monomerRef" v-model="monomerId" size="small" :project-id="globalProjectId" style="margin-bottom: 15px" />
    <div class="project-state-view-main">
      <div class="view-left">
        <common-radio-button v-model="projectType" default :options="monomerProductTypeEnum" type="enum" size="small" />
        <el-card class="left-item" v-loading="tableLoading">
          <div class="left-item-content">
            <el-progress type="circle" :percentage="Number(summaryInfo.completeRate)" :stroke-width="16" :width="210" :color="colors">
              <template #default="{ percentage }">
                <span style="font-size: 16px">{{ toFixed(percentage, 2) }}%</span>
              </template>
            </el-progress>
            <div class="left-item-detail">
              <div>
                <div style="margin-bottom: 5px">清单数：{{ summaryInfo.quantity }} {{ unitObj.measure }}</div>
                <div>生产数：{{ summaryInfo.completeQuantity }} {{ unitObj.measure }}</div>
              </div>
              <el-divider style="margin: 20px 0"></el-divider>
              <div>
                <div style="margin-bottom: 5px">清单量：{{ summaryInfo.totalMete }} {{ unitObj.unit }}</div>
                <div>生产量：{{ summaryInfo.completeMete }} {{ unitObj.unit }}</div>
              </div>
            </div>
          </div>
        </el-card>
      </div>
      <div class="view-center">
        <el-descriptions direction="vertical" :column="3" border>
          <el-descriptions-item align="center" label="开始生产">
            <span v-parse-time="{ val: globalProject.startDate, fmt: '{y}.{m}.{d}' }" />
          </el-descriptions-item>
          <el-descriptions-item align="center" label="完成时间">
            <span v-parse-time="{ val: globalProject.endDate, fmt: '{y}.{m}.{d}' }" />
          </el-descriptions-item>
          <el-descriptions-item align="center" label="累计耗时">
            <span class="tc-success">{{ diffDate }}</span>
          </el-descriptions-item>
        </el-descriptions>
        <div id="QCMain" class="QC-echarts"></div>
      </div>
      <div class="view-right" style="position: relative" v-loading="shipEchartsLoading">
        <el-date-picker
          v-model="month"
          type="month"
          size="small"
          class="date-item filter-item"
          style="width: 150px !important;position: absolute; top: 0px; left: 0px; z-index: 1"
          placeholder="选择月"
          format="YYYY-MM"
          value-format="YYYY-MM"
          @change="shipUpdateChart"
        />
        <div v-loading="qhseEchartsLoading" id="shipMain" class="ship-echarts"></div>
      </div>
    </div>
    <div v-loading="tableLoading">
      <el-descriptions direction="vertical" :column="4" border>
        <el-descriptions-item align="center" :label="`已生产(${unitObj.unit})`">
          {{ summaryInfo.completeMete }}
        </el-descriptions-item>
        <el-descriptions-item align="center" label="生产率">{{ summaryInfo.completeRate }}%</el-descriptions-item>
        <el-descriptions-item align="center" :label="`已发运(${unitObj.unit})`">
          {{ summaryInfo.shipMete }}
        </el-descriptions-item>
        <el-descriptions-item align="center" label="发运率">{{ summaryInfo.shipRate }}%</el-descriptions-item>
      </el-descriptions>
      <common-table show-summary :summary-method="getSummaries" :data="list" :max-height="maxHeight" style="width: 100%; margin-top: 15px">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" :show-overflow-tooltip="true" label="名称" align="center">
          <template #default="{ row }">
            <span>{{ row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="totalQuantity" :show-overflow-tooltip="true" label="总量" align="center">
          <template #default="{ row }">
            <span class="quantity-mete-show">
              <span class="left">{{ row.totalQuantity }} {{ unitObj.measure }} </span>
              <span class="line">|</span>
              <span class="right">{{ row.totalMete }} {{ unitObj.unit }}</span>
            </span>
          </template>
        </el-table-column>
        <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="已生产" align="center">
          <template #default="{ row }">
            <span class="quantity-mete-show">
              <span class="left">{{ row.completeQuantity }} {{ unitObj.measure }} </span>
              <span class="line">|</span>
              <span class="right">{{ row.completeMete }} {{ unitObj.unit }}</span>
            </span>
          </template>
        </el-table-column>
        <el-table-column prop="unCompleteQuantity" :show-overflow-tooltip="true" label="未生产" align="center">
          <template #default="{ row }">
            <span class="quantity-mete-show">
              <span class="left">{{ row.unCompleteQuantity }} {{ unitObj.measure }} </span>
              <span class="line">|</span>
              <span class="right">{{ row.unCompleteMete }} {{ unitObj.unit }}</span>
            </span>
          </template>
        </el-table-column>
        <el-table-column prop="shipQuantity" :show-overflow-tooltip="true" label="已发运" align="center">
          <template #default="{ row }">
            <span class="quantity-mete-show">
              <span class="left">{{ row.shipQuantity }} {{ unitObj.measure }} </span>
              <span class="line">|</span>
              <span class="right">{{ row.shipMete }} {{ unitObj.unit }}</span>
            </span>
          </template>
        </el-table-column>
        <el-table-column prop="unShipQuantity" :show-overflow-tooltip="true" label="未发运" align="center">
          <template #default="{ row }">
            <span class="quantity-mete-show">
              <span class="left">{{ row.unShipQuantity }} {{ unitObj.measure }} </span>
              <span class="line">|</span>
              <span class="right">{{ row.unShipMete }} {{ unitObj.unit }}</span>
            </span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getSummaryList } from '@/api/mes/production-manage/dashboard/project-dashboard'
import { computed, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import moment from 'moment'

import EO from '@enum'
import { projectComponentTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { dateDifference } from '@/utils/date'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useShipRecordCharts from '@compos/mes/production-manage/use-ship-record-charts'
import useQhseRecordCharts from '@compos/mes/production-manage/use-qhse-record-charts'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import monomerSelect from '@/components-system/plan/monomer-select'

const { maxHeight } = useMaxHeight({ extraBox: null, wrapperBox: ['.app-container'], extraHeight: 540, minHeight: 100 })

const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]
const initSummaryInfo = {
  quantity: 0,
  totalNetWeight: 0,
  totalLength: 0,
  totalMete: 0,
  completeQuantity: 0,
  completeNetWeight: 0,
  completeLength: 0,
  completeMete: 0,
  completeRate: 0,
  cargoQuantity: 0,
  shipQuantity: 0,
  cargoNetWeight: 0,
  shipMete: 0,
  shipRate: 0,
  cargoLength: 0
}
const monomerId = ref()
const projectType = ref()
const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])
const diffDate = computed(() => {
  return (globalProject.value && dateDifference(globalProject.value.startDate, globalProject.value.endDate)) || 0
})
const month = ref(moment(moment(globalProject.value.startDate)).format('YYYY-MM'))

const { updateChart: shipUpdateChart, echartsLoading: shipEchartsLoading } = useShipRecordCharts({
  elementId: 'shipMain',
  globalProjectId,
  monomerId,
  month
})
const { updateChart: qhseUpdateChart, echartsLoading: qhseEchartsLoading } = useQhseRecordCharts({
  elementId: 'QCMain',
  globalProjectId,
  monomerId
})

const monomerRef = ref()
const monomerProductTypeEnum = computed(() => {
  const _productType = monomerRef.value?.getProductType(monomerId.value) || 0
  return EO.getBits(projectComponentTypeEnum.ENUM, _productType)
})

const tableLoading = ref(false)
const list = ref([])
const summaryInfo = ref(initSummaryInfo)
const productType = computed(() => {
  return projectType.value & projectComponentTypeEnum.ARTIFACT.V ? componentTypeEnum.ARTIFACT.V : componentTypeEnum.ENCLOSURE.V
})

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({ productType: productType.value, w_unit: 'kg' })
})

async function fetchList() {
  if (!projectType.value || !monomerId.value || !globalProjectId.value) {
    return
  }
  try {
    list.value = []
    summaryInfo.value = initSummaryInfo
    tableLoading.value = true
    const content = await getSummaryList({
      projectId: globalProjectId.value,
      monomerId: monomerId.value,
      productType: productType.value,
      category: productType.value & componentTypeEnum.ARTIFACT.V ? undefined : projectType.value
    })
    summaryInfo.value = content.reduce((res, cur) => {
      for (const key in res) {
        res[key] += cur[key] || 0
      }
      return res
    }, initSummaryInfo)
    summaryInfo.value.totalMete = useProductMeteConvert({
      productType: productType.value,
      weight: { num: summaryInfo.value.totalNetWeight },
      length: { num: summaryInfo.value.totalLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    summaryInfo.value.completeMete = useProductMeteConvert({
      productType: productType.value,
      weight: { num: summaryInfo.value.completeNetWeight },
      length: { num: summaryInfo.value.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    summaryInfo.value.shipMete = useProductMeteConvert({
      productType: productType.value,
      weight: { num: summaryInfo.value.cargoNetWeight },
      length: { num: summaryInfo.value.cargoLength, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    summaryInfo.value.unCompleteQuantity = summaryInfo.value.quantity - summaryInfo.value.completeQuantity || 0
    summaryInfo.value.unShipQuantity = summaryInfo.value.quantity - summaryInfo.value.shipQuantity || 0
    summaryInfo.value.unCompleteMete = (summaryInfo.value.totalMete - summaryInfo.value.completeMete).toFixed(unitObj.value.DP) || 0
    summaryInfo.value.completeRate =
      (summaryInfo.value.totalMete && (summaryInfo.value.completeMete / summaryInfo.value.totalMete).toFixed(2)) || 0
    summaryInfo.value.unShipMete = (summaryInfo.value.totalMete - summaryInfo.value.shipMete).toFixed(unitObj.value.DP) || 0
    summaryInfo.value.shipRate = (summaryInfo.value.totalMete && (summaryInfo.value.shipMete / summaryInfo.value.totalMete).toFixed(2)) || 0
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.totalQuantity = v.quantity
      v.shipQuantity = v.cargoQuantity
      v.unCompleteQuantity = v.quantity - v.completeQuantity || 0
      v.unShipQuantity = v.quantity - v.shipQuantity || 0
      v.totalMete = useProductMeteConvert({
        productType: productType.value,
        weight: { num: v.totalNetWeight },
        length: { num: v.totalLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.completeMete = useProductMeteConvert({
        productType: productType.value,
        weight: { num: v.completeNetWeight },
        length: { num: v.completeLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.unCompleteMete = (v.totalMete - v.completeMete).toFixed(unitObj.value.DP) || 0
      v.shipMete = useProductMeteConvert({
        productType: productType.value,
        weight: { num: v.cargoNetWeight },
        length: { num: v.cargoLength, to: unitObj.value.unit, dp: unitObj.value.dp }
      })
      v.unShipMete = (v.totalMete - v.shipMete).toFixed(unitObj.value.DP) || 0
      return v
    })
    console.log(summaryInfo.value)
  } catch (error) {
    console.log('项目看板列表信息', error)
  } finally {
    tableLoading.value = false
  }
}

function getSummaries(param) {
  const { columns } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 2) {
      sums[index] = `${summaryInfo.value.quantity} ${unitObj.value.measure} | ${summaryInfo.value.totalMete} ${unitObj.value.unit}`
      return
    }
    if (index === 3) {
      sums[
        index
      ] = `${summaryInfo.value.completeQuantity} ${unitObj.value.measure} | ${summaryInfo.value.completeMete} ${unitObj.value.unit}`
      return
    }
    if (index === 4) {
      sums[
        index
      ] = `${summaryInfo.value.unCompleteQuantity} ${unitObj.value.measure} | ${summaryInfo.value.unCompleteMete} ${unitObj.value.unit}`
      return
    }
    if (index === 5) {
      sums[index] = `${summaryInfo.value.shipQuantity} ${unitObj.value.measure} | ${summaryInfo.value.shipMete} ${unitObj.value.unit}`
      return
    }
    if (index === 6) {
      sums[index] = `${summaryInfo.value.unShipQuantity} ${unitObj.value.measure} | ${summaryInfo.value.unShipMete} ${unitObj.value.unit}`
      return
    }
  })
  return sums
}

watch(
  () => [monomerId.value, projectType.value],
  () => {
    fetchList()
    shipUpdateChart()
    qhseUpdateChart()
  },
  { immediate: true, deep: true }
)
</script>

<style lang="scss" scoped>
.project-state-view-main {
  display: flex;
  overflow: auto;
  margin-bottom: 15px;

  & > div > :not(:last-child) {
    margin-bottom: 15px;
  }
}

.view-left {
  flex: 0 0 470px;

  .left-item {
    display: flex;
    flex-direction: column;
    width: 100%;
    .left-item-content {
      margin: 15px 0px;
      display: flex;
      align-items: center;

      .left-item-detail {
        width: 100%;
        margin-left: 15px;
        font-size: 18px;

        & > :not(:last-child) {
          margin-bottom: 15px;
        }
      }
    }
  }
}

.view-center {
  padding: 0 15px;
  box-sizing: border-box;
  flex: 1;

  .QC-echarts {
    width: 100%;
    height: 235px;
  }
}

.view-right {
  flex: 1;
  .ship-echarts {
    width: 100%;
    height: 350px;
  }
}
</style>

<style lang="scss">
// .project-state-view-main .el-card__body {
//   padding: 10px;
// }

.project-dashboard .el-descriptions__content {
  font-size: 25px;
  font-weight: 700;
}

.quantity-mete-show {
  display: flex;

  .left {
    width: 40%;
    text-align: right;
  }

  .right {
    width: 60%;
    text-align: left;
  }

  .line {
    width: 15px;
  }
}
</style>

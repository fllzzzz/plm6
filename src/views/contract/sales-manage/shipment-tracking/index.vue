<template>
  <div class="app-container">
    <div class="head-container common-container">
      <common-radio-button
        v-model="productType"
        :options="installProjectTypeEnum.ENUM"
        :unshowVal="[installProjectTypeEnum.ENCLOSURE.V]"
        default
        type="enum"
        size="small"
        class="filter-item"
      />
      <el-date-picker
        v-model="statisticalTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        :disabled-date="disabledDate"
        :clearable="false"
        style="width: 230px"
      />
      <el-row v-if="checkPermission(permission.summary)" v-loading="summaryLoading" :gutter="10" class="panel-group">
        <el-col class="card-panel-col">
          <Panel
            :name="`累计发运量（吨）`"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryData.shipMet || 0"
            :precision="2"
            :show-empty="productType === installProjectTypeEnum.AUXILIARY.V"
          />
        </el-col>
        <el-col class="card-panel-col">
          <Panel name="累计发运额（元）" text-color="#626262" num-color="#1890ff" :end-val="summaryData.shipAmount || 0" :precision="2" />
        </el-col>
        <el-col class="card-panel-col">
          <Panel
            name="累计车次"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryData.cargoQuantity || 0"
            :show-empty="productType === installProjectTypeEnum.AUXILIARY.V"
          />
        </el-col>
        <el-col class="card-panel-col">
          <Panel
            :name="`筛选日期发运量（吨）`"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryData.shipMetTime || 0"
            :precision="2"
            :show-empty="productType === installProjectTypeEnum.AUXILIARY.V"
          />
        </el-col>
        <el-col class="card-panel-col">
          <Panel
            name="筛选日期发运额（元）"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryData.shipAmountTime || 0"
            :precision="2"
          />
        </el-col>
      </el-row>
    </div>
    <component :is="currentView" @reset-query="resetQuery" />
  </div>
</template>

<script setup>
import { shipSummary } from '@/api/contract/sales-manage/shipment-tracking'
import { ref, computed, provide, watch } from 'vue'
import { mapGetters } from '@/store/lib'

import { shipmentTrackingPM as permission } from '@/page-permission/contract'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { installProjectTypeEnum } from '@enum-ms/project'
import { isBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import moment from 'moment'

import structure from './structure'
import enclosure from './enclosure'
import auxiliaryMaterial from './auxiliary-material'
import Panel from '@/components/Panel'

// 当前显示组件
const currentView = computed(() => {
  switch (productType.value) {
    case installProjectTypeEnum.ENCLOSURE.V:
      return enclosure
    case installProjectTypeEnum.AUXILIARY.V:
      return auxiliaryMaterial
    default:
      return structure
  }
})

const { globalProjectId } = mapGetters(['globalProjectId'])

const productType = ref()
const summaryLoading = ref(false)
const summaryData = ref({})

const times = PICKER_OPTIONS_SHORTCUTS[1]?.value()
const statisticalTime = ref([moment(times[0]).valueOf(), moment(times[1]).valueOf()])

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

// 公共参数
const commonParams = computed(() => {
  return {
    startDate: statisticalTime.value[0],
    endDate: statisticalTime.value[1],
    projectId: globalProjectId.value,
    productType: productType.value
  }
})

provide('commonParams', commonParams)

watch(
  commonParams,
  () => {
    fetchSummary()
  },
  { immediate: true, deep: true }
)

function disabledDate(time) {
  return time > new Date()
}

// 重置
function resetQuery() {
  statisticalTime.value = [moment(times[0]).valueOf(), moment(times[1]).valueOf()]
}

// 获取发运汇总
async function fetchSummary() {
  if (!checkPermission(permission.summary) || isBlank(globalProjectId.value)) {
    summaryData.value = {}
    return
  }
  try {
    summaryLoading.value = true
    summaryData.value = (await shipSummary(commonParams.value)) || {}
  } catch (error) {
    console.log(error)
  } finally {
    summaryLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.panel-group {
  display: flex;
  .card-panel-col {
    flex: 1;
    min-width: 0;
  }
  ::v-deep(.card-panel) {
    .card-panel-description {
      margin: 10px 20px;
      display: flex;
      flex-direction: row;
      justify-content: space-between;
      align-items: flex-start;
      flex-wrap: wrap;
      .card-panel-text {
        margin-top: 2px;
      }
      .card-panel-num {
        font-size: 20px;
      }
    }
  }
}
</style>

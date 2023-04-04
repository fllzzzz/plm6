<template>
  <div class="app-container">
    <div class="head-container common-container">
      <common-radio-button
        v-model="productType"
        :options="installProjectTypeEnum.ENUM"
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
        style="width:230px"
        @change="fetchSummary"
      />
      <el-row v-loading="summaryLoading" :gutter="10" class="panel-group">
        <el-col class="card-panel-col">
          <Panel :name="`累计发运量（吨）`" text-color="#626262" num-color="#1890ff" :end-val="projectSummary.beginPeriod || 0" :precision="2" />
        </el-col>
        <el-col class="card-panel-col">
          <Panel name="累计发运额（元）" text-color="#626262" num-color="#1890ff" :end-val="projectSummary.inbound || 0" :precision="2" />
        </el-col>
        <el-col class="card-panel-col">
          <Panel name="累计车次（元）" text-color="#626262" num-color="#1890ff" :end-val="projectSummary.outbound || 0" :precision="2" />
        </el-col>
        <el-col class="card-panel-col">
          <Panel :name="`筛选日期发运量（吨）`" text-color="#626262" num-color="#1890ff" :end-val="projectSummary.endPeriod || 0" :precision="2" />
        </el-col>
        <el-col class="card-panel-col">
          <Panel name="筛选日期发运额（元）" text-color="#626262" num-color="#1890ff" :end-val="projectSummary.endPeriod || 0" :precision="2" />
        </el-col>
      </el-row>
    </div>
    <component :is="currentView" ref="domRef" />
  </div>
</template>

<script setup>
import { cost } from '@/api/contract/sales-manage/price-manage/common'
import { ref, computed, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { transactionRecordPM as permission } from '@/page-permission/contract'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { installProjectTypeEnum } from '@enum-ms/project'
import { isBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'

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

const domRef = ref()
const projectId = ref()
const productType = ref()
const summaryLoading = ref(false)
const monomerId = ref()
const projectSummary = ref({})
const statisticalTime = ref(PICKER_OPTIONS_SHORTCUTS[1]?.value())

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

provide('statisticalTime', statisticalTime)
provide('monomerId', monomerId)
provide('projectId', globalProjectId)

function disabledDate(time) {
  return time > new Date()
}

fetchSummary()

// 获取项目汇总
function fetchSummary() {
  if (!checkPermission(permission.summary) || isBlank(projectId.value)) {
    projectSummary.value = {}
    return
  }
  try {
    summaryLoading.value = true
    const params = {
      statisticalTime: statisticalTime.value,
      projectId: projectId.value
    }
    projectSummary.value = cost(params)
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

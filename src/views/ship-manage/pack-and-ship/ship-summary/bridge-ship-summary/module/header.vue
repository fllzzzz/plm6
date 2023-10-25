<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.dateTime"
        type="year"
        format="YYYY"
        value-format="x"
        placeholder="请选择"
        style="width: 100px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <!-- <common-radio-button
        v-model="query.productionLineTypeEnum"
        :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
        showOptionAll
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <common-radio-button
        v-model="query.status"
        :options="[shipStatusEnum.SHIPPING, shipStatusEnum.SHIPPED]"
        showOptionAll
        type="enum"
        class="filter-item"
        @change="statusChange"
      />
      <workshop-select
        v-model="query.workshopId"
        placeholder="请选择车间"
        :workshop-type="workshopTypeEnum.BRIDGE.V"
        clearable
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
        <el-col :span="12" class="card-panel-col">
          <Panel
            name="累计发运量（t）"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryInfo.mete / 1000 || 0"
            :precision="DP.COM_WT__T"
          />
        </el-col>
        <el-col :span="12" class="card-panel-col">
          <Panel name="累计车次" text-color="#626262" num-color="#1890ff" :end-val="summaryInfo.quantity || 0" :precision="0" />
        </el-col>
      </el-row>
      <common-radio-button
        type="enum"
        v-model="query.weightStatus"
        :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.projectName"
        placeholder="项目搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { shipmentSummary } from '@/api/ship-manage/pack-and-ship/bridge-ship-summary'
import workshopSelect from '@comp-mes/workshop-select'
import { regHeader } from '@compos/use-crud'
import { shipStatusEnum } from '@enum-ms/mes'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import rrOperation from '@crud/RR.operation'
import moment from 'moment'
import { DP } from '@/settings/config'
// import checkPermission from '@/utils/system/check-permission'

import Panel from '@/components/Panel'

const defaultQuery = {
  dateTime: moment().valueOf(),
  productionLineTypeEnum: undefined,
  workshopId: undefined,
  sendStatus: undefined,
  status: undefined,
  settled: undefined,
  projectName: undefined,
  weightStatus: weightTypeEnum.NET.V
}
const { crud, query } = regHeader(defaultQuery)

const summaryInfo = ref({})
const summaryLoading = ref(false)

function statusChange(val) {
  if (val === shipStatusEnum.SETTLED.V) {
    query.sendStatus = undefined
    query.settled = 1
  } else {
    query.sendStatus = val
    query.settled = undefined
  }
  crud.toQuery()
}

watch(
  () => query,
  (val) => {
    if (val) {
      getData()
    }
  },
  { immediate: true, deep: true }
)

async function getData() {
  try {
    summaryLoading.value = true
    const data = await shipmentSummary(query)
    summaryInfo.value = data || {}
  } catch (error) {
    console.log('获取累计发运', error)
  } finally {
    summaryLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        margin-top: 2px;
      }
      .card-panel-num {
        display: block;
        font-size: 20px;
      }
    }
  }
}
</style>

<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.dateTime"
        type="year"
        format="YYYY"
        value-format="x"
        placeholder="请选择"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.status"
        :options="shipStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="statusChange"
      />
      <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
        <el-col :span="12" class="card-panel-col">
          <Panel name="累计发运量（t）" text-color="#626262" num-color="#1890ff" :end-val="(totalAmount.mete/1000) || 0" :precision="DP.COM_WT__T" />
        </el-col>
        <el-col :span="12" class="card-panel-col">
          <Panel name="累计车次" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.quantity || 0" :precision="0" />
        </el-col>
      </el-row>
    </div>
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { shipmentSummary } from '@/api/mes/pack-and-ship/ship-summary'

import { regHeader } from '@compos/use-crud'
import { shipStatusEnum } from '@enum-ms/mes'
import moment from 'moment'
import { DP } from '@/settings/config'
// import checkPermission from '@/utils/system/check-permission'

import Panel from '@/components/Panel'

const defaultQuery = {
  dateTime: moment().valueOf(),
  sendStatus: undefined,
  settled: undefined
}
const { crud, query } = regHeader(defaultQuery)

const totalAmount = ref({})

function statusChange(val) {
  if (query.status === shipStatusEnum.SETTLED.V) {
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
    const data = await shipmentSummary(query)
    totalAmount.value = data || {}
  } catch (error) {
    console.log('获取累计发运', error)
  }
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 18px;
      }
    }
  }
}
</style>

<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        format="YYYY"
        value-format="YYYY"
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
        <el-col :span="8" class="card-panel-col">
          <Panel name="构件累计发运量（t）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.artifactShipMete || 0" :precision="0" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="围护累计发运量（m）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.enclosureShipMete || 0" :precision="0" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="累计车次" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.shipCarQuantity || 0" :precision="0" />
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
// import checkPermission from '@/utils/system/check-permission'

import Panel from '@/components/Panel'

const defaultQuery = {
  year: new Date().getFullYear(),
  shipStatus: undefined,
  settlementStatus: undefined
}
const { crud, query } = regHeader(defaultQuery)

const totalAmount = ref({})

function statusChange(val) {
  if (query.status === shipStatusEnum.SETTLED.V) {
    query.shipStatus = undefined
    query.settlementStatus = 1
  } else {
    query.shipStatus = val
    query.settlementStatus = undefined
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

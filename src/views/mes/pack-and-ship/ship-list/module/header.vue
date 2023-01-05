<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.manufactureType"
        :options="manufactureTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <component-radio-button
        v-model="query.productType"
        :options="packTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-if="query.productType & packTypeEnum.ENCLOSURE.V"
        v-model="query.category"
        :options="mesEnclosureTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.deliveryStatus"
        :options="deliveryStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.deliveryDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        @change="handleDateChange"
      />
      <div>
        <el-input
          v-model="query.blurry"
          placeholder="产品名称/产品编号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入车次搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="可输入车牌搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.driverName"
          placeholder="可输入司机搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.auditUserName"
          placeholder="可输入办理人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input-number
          v-model.number="query.minNum"
          :min="0"
          :max="query.maxNum || 999"
          :precision="2"
          :controls="false"
          size="small"
          style="width: 80px"
          class="filter-item"
          placeholder="差值率"
        />%~<el-input-number
          v-model.number="query.maxNum"
          class="filter-item"
          :min="query.minNum || 0"
          :precision="2"
          :max="999"
          :controls="false"
          size="small"
          style="width: 80px"
          placeholder="差值率"
        />%
        <rrOperation />
      </div>
    </div>
    <el-row v-loading="crud.loading" :gutter="20" class="panel-group">
      <el-col :span="6" class="card-panel-col">
        <Panel
          name="今年发运量（t）"
          text-color="#626262"
          num-color="#1890ff"
          :end-val="convertUnits(totalAmount.yearMete, 'kg', 't', DP.COM_WT__T) || 0"
          :precision="DP.COM_WT__T"
        />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel name="今年发运车次" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.yearQuantity || 0" :precision="0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel
          name="本月发运量（t）"
          text-color="#626262"
          num-color="#1890ff"
          :end-val="convertUnits(totalAmount.monthMete, 'kg', 't', DP.COM_WT__T) || 0"
          :precision="DP.COM_WT__T"
        />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <Panel name="本月发运车次" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.monthQuantity || 0" :precision="0" />
      </el-col>
    </el-row>
    <crudOperation>
      <template v-slot:optLeft>
        <print-table
          v-permission="[...permission.print, ...permission.detailPrint]"
          v-model:current-key="currentKey"
          :api-key="apiKey"
          :params="printParams"
          :before-print="handleBeforePrint"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
      <template v-slot:viewLeft>
        <el-tag v-permission="permission.get" effect="plain" class="filter-item" size="medium">
          累计发运重量（查询）：
          <span v-if="!summaryLoading">{{ convertUnits(shipWeight, 'kg', 't', DP.COM_WT__T) }} t</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { getSummaryShipMete, getSummaryMonthMete } from '@/api/mes/pack-and-ship/ship-list'
import { inject, onMounted, ref, computed } from 'vue'
import moment from 'moment'

import { packTypeEnum, mesEnclosureTypeEnum, deliveryStatusEnum } from '@enum-ms/mes'
import { manufactureTypeEnum } from '@enum-ms/production'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { isBlank, isNotBlank } from '@data-type/index'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Panel from '@/components/Panel'
import { ElMessage } from 'element-plus'

const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  driverName: undefined,
  auditUserName: undefined,
  auditStartDate: undefined,
  auditEndDate: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false },
  manufactureType: { value: undefined, resetAble: false },
  minNum: undefined,
  maxNum: undefined
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const permission = inject('permission')
const summaryLoading = ref(false)
const shipWeight = ref(0)
const currentKey = ref()
const apiKey = ref([])
const totalAmount = ref({})

onMounted(() => {
  if (checkPermission(permission.print)) {
    apiKey.value.push('mesShipmentSummary')
  }
  if (checkPermission(permission.detailPrint)) {
    apiKey.value.push('mesShipmentDetail')
  }
})

const printParams = computed(() => {
  if (currentKey.value === 'mesShipmentSummary') {
    return { ...query }
  }
  if (currentKey.value === 'mesShipmentDetail' && isNotBlank(crud.selections)) {
    return crud.selections.map((row) => {
      return row.id
    })
  }
  return undefined
})

function handleBeforePrint() {
  if (currentKey.value === 'mesShipmentDetail' && isBlank(printParams.value)) {
    ElMessage.warning('至少选择一条需要打印的发运信息')
    return false
  }
}

CRUD.HOOK.afterToQuery = () => {
  fetchSummary()
  fetchMonthSummary()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.acceptDifference = v.totalNetWeight - v.actualWeight >= 0
    v.difference = Math.abs(v.totalNetWeight - v.actualWeight)
    v.differenceRate = v.totalNetWeight ? ((v.difference / v.totalNetWeight) * 100).toFixed(1) + '%' : '-'
    return v
  })
}
async function fetchSummary() {
  if (!checkPermission(permission.get)) {
    return
  }
  try {
    summaryLoading.value = true
    const { weight } = await getSummaryShipMete(query)
    shipWeight.value = weight
  } catch (error) {
    console.log('获取汇总信息', error)
  } finally {
    summaryLoading.value = false
  }
}

async function fetchMonthSummary() {
  try {
    const data = await getSummaryMonthMete({ dateTime: new Date().getTime() })
    totalAmount.value = data || {}
  } catch (e) {
    console.log('获取发运记录汇总失败', e)
  }
}

function handleDateChange() {
  if (query.deliveryDate && query.deliveryDate.length > 1) {
    query.auditStartDate = moment(query.deliveryDate[0]).valueOf()
    query.auditEndDate = moment(query.deliveryDate[1]).valueOf()
  } else {
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
  crud.toQuery()
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align: left;
        margin-top: 2px;
      }
      .card-panel-num {
        display: block;
        font-size: 18px;
        text-align: right;
      }
    }
  }
}
</style>

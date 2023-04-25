<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button v-model="query.type" :options="timeTypeEnum.ENUM" class="filter-item" type="enum" @change="handleChange" />
      <el-date-picker
        v-if="query.type === timeTypeEnum.ALL_YEAR.V"
        v-model="query.dateTime"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择年"
        format="YYYY"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="handleYearChange"
      />
      <el-date-picker
        v-if="query.type === timeTypeEnum.CURRENT_MONTH.V"
        v-model="query.dateTime"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="handleMonthChange"
      />
      <workshop-select
        v-model="query.workshopId"
        placeholder="选择车间"
        style="width: 200px"
        class="filter-item"
        clearable
        :factory-id="query.factoryId"
        @change="handleWorkshopChange"
      />
      <el-row v-loading="summaryLoading" v-permission="permission.statistics" :gutter="24" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="排产总量(吨)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="summaryInfo.schedulingTotalNetWeight / 1000 || 0"
            :precision="2"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="排产项目数(个)" text-color="#626262" num-color="#F56C6C" :endVal="summaryInfo.projectQuantity || 0" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="完成量(吨)"
            text-color="#626262"
            num-color="#F56C6C"
            :endVal="summaryInfo.completeTotalNetWeight / 1000 || 0"
            :precision="2"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="完成率(%)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="
              summaryInfo.schedulingTotalNetWeight ? (summaryInfo.completeTotalNetWeight / summaryInfo.schedulingTotalNetWeight) * 100 : 0
            "
            :precision="2"
          />
        </el-col>
      </el-row>
    </div>
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { getScheduleSummary } from '@/api/mes/scheduling-manage/scheduling-data.js'
import { timeTypeEnum } from '@enum-ms/contract'
import { regHeader } from '@compos/use-crud'
import { schedulingDataPM as permission } from '@/page-permission/mes'
// import { parseTime } from '@/utils/date'
import workshopSelect from '@comp-mes/workshop-select'
import Panel from '@/components/Panel'
import moment from 'moment'

const summaryLoading = ref(false)
const summaryInfo = ref({})
const defaultQuery = {
  dateTime: moment().startOf('year').valueOf(),
  type: timeTypeEnum.ALL_YEAR.V,
  workshopId: undefined
}

const { crud, query } = regHeader(defaultQuery)

function handleChange(val) {
  if (val === timeTypeEnum.ALL_YEAR.V) {
    query.dateTime = moment().startOf('year').valueOf()
  } else {
    query.dateTime = moment().startOf('month').valueOf()
  }
  fetchSummary()
  crud.data = []
  crud.toQuery()
}

watch(
  () => crud.query,
  (val) => {
    fetchSummary()
  },
  { immediate: true }
)

function disabledDate(time) {
  return new Date() < time
}

async function fetchSummary() {
  summaryLoading.value = true
  try {
    const data = await getScheduleSummary({
      type: query.type,
      dateTime: query.dateTime,
      workshopId: query.workshopId
    })
    summaryInfo.value = data || {}
  } catch (err) {
    console.log('获取排产数据汇总失败', err)
  } finally {
    summaryLoading.value = false
  }
}

function handleYearChange() {
  fetchSummary()
  crud.toQuery()
}

function handleMonthChange() {
  fetchSummary()
  crud.toQuery()
}

function handleWorkshopChange() {
  fetchSummary()
  crud.toQuery()
}
</script>

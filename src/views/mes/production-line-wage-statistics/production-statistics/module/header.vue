<template>
  <div class="head-container">
    <el-date-picker
      v-model="query.dateTime"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      placeholder="选择年"
      format="YYYY"
      :clearable="false"
      value-format="x"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <el-row v-loading="summaryLoading" v-permission="permission.statistics" :gutter="20" class="panel-group">
      <el-col :span="8" class="card-panel-col">
        <Panel name="产量（吨）" text-color="#626262" num-color="#1890ff" :endVal="summaryInfo.mete / 1000 || 0" :precision="2" />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel name="工资总额（元）" text-color="#626262" num-color="#1890ff" :endVal="summaryInfo.price || 0" :precision="2" />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel
          name="平均单价（元/吨）"
          text-color="#626262"
          num-color="#1890ff"
          :end-val="summaryInfo.mete? summaryInfo.price?.toFixed(2) / ((summaryInfo.mete / 1000)?.toFixed(2)) : 0 || 0"
          :precision="2"
        />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import { getSummary } from '@/api/mes/production-line-wage-statistics/production-statistics'
import Panel from '@/components/Panel'
import moment from 'moment'

const defaultTime = moment().valueOf().toString()
const permission = inject('permission')

const summaryLoading = ref(false)
const summaryInfo = ref({})
const defaultQuery = {
  dateTime: defaultTime.toString()
}

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.afterToQuery = () => {
  fetchSummary()
}

async function fetchSummary() {
  try {
    const data = await getSummary({ dateTime: crud.query.dateTime })
    summaryInfo.value = data || {}
  } catch (error) {
    console.log('根据时间获取全年价格汇总', error)
  }
}

function disabledDate(time) {
  return time > new Date()
}

</script>

<style>
</style>

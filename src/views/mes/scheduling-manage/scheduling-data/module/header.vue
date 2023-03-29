<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button v-model="query.timeType" :options="timeTypeEnum.ENUM" class="filter-item" type="enum" @change="handleChange" />
      <el-date-picker
        v-if="query.timeType === timeTypeEnum.ALL_YEAR.V"
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-if="query.timeType === timeTypeEnum.CURRENT_MONTH.V"
        v-model="query.month"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="YYYY-MM"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <workshop-select
        v-model="query.workshopId"
        placeholder="选择车间"
        style="width: 200px"
        class="filter-item"
        :factory-id="query.factoryId"
        @change="crud.toQuery"
      />
      <el-row v-loading="summaryLoading" v-permission="permission.get" :gutter="24" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel name="排产总量(吨)" text-color="#626262" num-color="#1890ff" :end-val="0" :precision="DP.COM_WT__KG" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="排产项目数(个)" text-color="#626262" num-color="#F56C6C" :endVal="0" :precision="DP.COM_WT__KG" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="完成量(吨)" text-color="#626262" num-color="#F56C6C" :endVal="0" :precision="DP.COM_WT__KG" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="完成率(%)" text-color="#626262" num-color="#1890ff" :end-val="0" :precision="DP.COM_WT__KG" />
        </el-col>
      </el-row>
    </div>
  </div>
</template>

<script setup>
import { ref, inject } from 'vue'
import { timeTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { regHeader } from '@compos/use-crud'
import workshopSelect from '@comp-mes/workshop-select'
import Panel from '@/components/Panel'

const summaryLoading = ref(false)
const permission = inject('permission')
const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  month: undefined,
  timeType: timeTypeEnum.ALL_YEAR.V
}

const { crud, query } = regHeader(defaultQuery)

function handleChange(val) {
  if (val === timeTypeEnum.ALL_YEAR.V) {
    query.year = parseTime(new Date(), '{y}')
    query.month = undefined
  } else {
    query.year = parseTime(new Date(), '{y}')
    query.month = parseTime(new Date(), '{y}-{m}')
  }
  crud.toQuery()
}
</script>

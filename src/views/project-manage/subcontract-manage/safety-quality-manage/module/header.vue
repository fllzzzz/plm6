<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
      <common-radio-button
        v-model="query.problemTypeId"
        :options="qualityProblemType"
        showOptionAll
        :optionAllValue="undefined"
        type="other"
        :loading="!loaded"
        :dataStructure="{key: 'id', label: 'name', value: 'id'}"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.boolChangeStatus"
        :options="qualityProblemChangeType.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.projectName"
        placeholder="项目"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="分包单位"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.promoterName"
        placeholder="发起人"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { qualityProblemChangeType } from '@enum-ms/project'
import useQualityProblemType from '@compos/store/use-quality-problem-type'

const { loaded, qualityProblemType } = useQualityProblemType()

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  problemTypeId: undefined,
  boolChangeStatus: undefined,
  projectName: undefined,
  supplierName: undefined,
  promoterName: undefined
}

const { crud, query } = regHeader(defaultQuery)
// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

</script>

<template>
  <div class="head-container">
    <div style="display: flex; justify-content: space-between;">
      <div>
        <el-date-picker
          class="filter-item"
          type="daterange"
          range-separator="-"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          v-model="query.date"
          value-format="x"
          unlink-panels
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          @change="handleDateChange" />
        <monomer-select-area-select v-model:monomerId="query.monomerId" v-model:areaId="query.areaId" @change="crud.toQuery" :project-id="query.projectId" clearable />
        <el-input class="filter-item" style="width: 200px;" v-model="query.serialNumber" @keyup.enter="crud.toQuery" placeholder="请输入搜索编号" clearable />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click="searchQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click="resetQuery">重置</common-button>
      </div>
      <export-button v-permission="permission.download" class="filter-item" :fn="componentChangeDownload" :params="query">下载成品变更清单</export-button>
    </div>
  </div>
</template>
<script setup>
// import {} from 'vue'
// import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
import moment from 'moment'
import ExportButton from '@comp-common/export-button/index.vue'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import { finishedProductChangePM as permission } from '@/page-permission/mes'
import { componentChangeDownload } from '@/api/mes/changed-manage/finished-product-change'
import { regHeader } from '@compos/use-crud'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf(),
  monomerId: undefined,
  projectId: undefined,
  areaId: undefined,
  serialNumber: undefined
}

const { crud, query } = regHeader(defaultQuery)

useGlobalProjectIdChangeToQuery(crud)

const handleDateChange = (v) => {
  if (v && v.length > 1) {
    query.startDate = v[0]
    query.endDate = v[1]
  } else {
    query.startDate = ''
    query.endDate = ''
  }
  crud.toQuery()
}

const searchQuery = () => {
  crud.toQuery()
}

const resetQuery = () => {
  query.monomerId = undefined
  query.areaId = undefined
  query.serialNumber = undefined
  query.startDate = moment().startOf('month').valueOf()
  query.endDate = moment().valueOf()
  query.date = [moment().startOf('month').valueOf(), moment().valueOf()]
  crud.toQuery()
}

</script>

<template>
  <div class="app-container">
    <div class="head-container" style="padding-bottom: 0px">
      <monomer-select-area-tabs :project-id="globalProjectId" @change="fetchMonomerAndArea" />
      <common-radio-button
        class="filter-item"
        v-model="commonQuery.category"
        :options="projectComponentTypeEnum.ENUM"
        type="enum"
        size="small"
      />
      <el-date-picker
        v-model="commonQuery.date"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px"
        :clearable="false"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        value-format="x"
        @change="handleDateChange"
      />
    </div>
    <component ref="componentRef" :is="currentView" />
  </div>
</template>

<script setup>
import { reactive, computed, ref, provide } from 'vue'
import { mapGetters } from '@/store/lib'

import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { projectComponentTypeEnum } from '@enum-ms/mes'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import artifactComponent from './artifact'
import enclosureComponent from './enclosure'

const commonQuery = reactive({
  category: projectComponentTypeEnum.ARTIFACT.V,
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf()
})

const { globalProjectId } = mapGetters(['globalProjectId'])
provide('commonQuery', commonQuery)

const componentRef = ref()
const currentView = computed(() => {
  if (commonQuery.category === projectComponentTypeEnum.ARTIFACT.V) {
    return artifactComponent
  } else {
    return enclosureComponent
  }
})

function handleDateChange() {
  if (commonQuery.date && commonQuery.date.length > 1) {
    commonQuery.startDate = commonQuery.date[0]
    commonQuery.endDate = commonQuery.date[1]
  } else {
    commonQuery.startDate = undefined
    commonQuery.endDate = undefined
  }
  componentRef.value.toQuery()
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  commonQuery.monomerId = monomerId
  commonQuery.areaId = areaId
  componentRef.value.toQuery()
}
</script>

<template>
  <div class="head-container">
    <div v-show="crud.searchToggle" style="display: flex; justify-content: space-between">
      <div>
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item date-item"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          value-format="x"
          style="width: 240px"
          @change="handleDateChange"
        />
        <common-radio-button
          type="enum"
          v-model="query.status"
          :options="[projectStatusEnum.PROCESS, projectStatusEnum.COMPLETE, projectStatusEnum.SUSPEND]"
          show-option-all
          class="filter-item"
          @change="crud.toQuery"
        />
        <common-radio-button
          type="enum"
          v-model="query.weightStatus"
          :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
          class="filter-item"
          @change="crud.toQuery"
        />
        <workshop-select
          v-model="query.workshopId"
          :workshop-type="workshopTypeEnum.BUILDING.V"
          placeholder="请选择车间"
          clearable
          style="width: 200px"
          class="filter-item"
          @change="crud.toQuery"
        />
      </div>
      <div>
        <slot name="viewLeft"></slot>
      </div>
    </div>
  </div>
</template>
<script setup>
import { regHeader } from '@compos/use-crud'
import { projectStatusEnum } from '@enum-ms/contract'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import workshopSelect from '@comp-mes/workshop-select'
import moment from 'moment'

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf(),
  status: projectStatusEnum.PROCESS.V,
  weightStatus: weightTypeEnum.NET.V,
  workshopId: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 时间变动
function handleDateChange(val) {
  if (val && val.length > 1) {
    query.startDate = val[0]
    query.endDate = val[1]
  } else {
    query.startDate = moment().startOf('month').valueOf()
    query.endDate = moment().valueOf()
  }
  crud.toQuery()
}
</script>

<style lang="scss" scoped>
</style>

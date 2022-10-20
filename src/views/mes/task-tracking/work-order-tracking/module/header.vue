<template>
  <div class="head-container">
     <el-date-picker
        v-model="query.month"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        format="YYYY/MM"
        value-format="YYYY/MM"
        placeholder="选择月"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workshopInfId"
        placeholder="请选择车间"
        :factory-id="query.factoryId"
        style="width: 270px"
        class="filter-item"
        defaultValue
      />
      <production-line-select
        ref="productionLineRef"
        class="filter-item"
        v-model="query.productionLineId"
        :factory-id="query.factoryId"
        placeholder="请选择生产线"
        style="width: 270px"
        clearable
        defaultValue
      />
        <common-radio-button
        v-model="query.productType"
        :options="[processMaterialListTypeEnum.ARTIFACT,processMaterialListTypeEnum.MACHINE_PART]"
        class="filter-item"
        type="enum"
        @change="productEnum"
      />
        <common-radio-button
        v-model="query.schedulingStatus"
        :options="taskTrackingSchedulingStatusEnum.ENUM"
        showOptionAll
        class="filter-item"
        type="enum"
        @change="crud.toQuery"
      />
  </div>
</template>

<script setup>
import { defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import { processMaterialListTypeEnum, taskTrackingSchedulingStatusEnum } from '@enum-ms/mes'
import moment from 'moment'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'

// const transformTab = ref(componentTypeEnum.ARTIFACT.V)
const emit = defineEmits(['change'])
const defaultQuery = {
  month: parseTime(new Date(), '{y}/{m}'),
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
  endDate: moment().valueOf(),
  productType: processMaterialListTypeEnum.ARTIFACT.V,
  schedulingStatus: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

function productEnum(val) {
  emit('change', val)
}

</script>

<style>

</style>

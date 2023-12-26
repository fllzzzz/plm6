<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.projectWarehouseType"
        :options="projectWarehouseTypeEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <project-cascader
        v-if="query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V"
        v-model="query.projectId"
        placeholder="所属项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 260px"
      />
      <common-radio-button
        v-if="props.showType !== 'coilPlate'"
        type="enum"
        v-model="query.status"
        :options="reviewStatusEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
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
      <el-input
        v-model.trim="query.receiptSerialNumber"
        placeholder="转换单号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <el-input
      v-if="props.showType !== 'coilPlate'"
        v-model.trim="query.outSerialNumber"
        placeholder="出库单号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <!-- <project-cascader
        v-model="query.projectId"
        placeholder="所属项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      /> -->
      <el-input
        v-model.trim="query.brand"
        placeholder="品牌"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { defineProps } from 'vue'
import { reviewStatusEnum } from '@enum-ms/common'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const props = defineProps({
  showType: {
    type: String,
    default: undefined
  }
})

const defaultQuery = {
  date: [],
  status: props.showType === 'coilPlate' ? { value: reviewStatusEnum.UNREVIEWED.V, resetAble: false } : reviewStatusEnum.UNREVIEWED.V,
  startDate: undefined,
  endDate: undefined,
  projectId: undefined,
  receiptSerialNumber: undefined,
  outSerialNumber: undefined,
  specification: undefined,
  brand: undefined
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

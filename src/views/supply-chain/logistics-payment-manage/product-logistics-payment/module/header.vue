<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
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
            v-model="query.projectStatus"
            :options="productLProjectStatusEnum.ENUM"
            showOptionAll
            :optionAllValue="undefined"
            type="enum"
            class="filter-item"
            @change="crud.toQuery"
            style="float:left;"
          />
          <el-input
            v-model="query.supplierName"
            placeholder="物流公司"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation/>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { productLProjectStatusEnum } from '@enum-ms/contract'

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  projectStatus: undefined,
  supplierName: undefined
}
const { crud, query } = regHeader(defaultQuery)

// 时间变动
function handleDateChange(val) {
  if (query.date && query.date.length > 1) {
    query.startDate = val[0]
    query.endDate = val[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

</script>

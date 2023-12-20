<template>
  <div class="head-container">
    <div>
      <el-input
          placeholder="可输入编号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          placeholder="可输入规格搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          placeholder="输入材质搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <rrOperation />
    </div>
      <crudOperation>
      <template #optRight>
        <el-tag size="medium" effect="plain">
          统计日期：
          <span v-parse-time="{ val: query.startDate, fmt: '{y}-{m}-{d}' }" />
          ~
          <span v-parse-time="{ val: query.endDate, fmt: '{y}-{m}-{d}' }" />
        </el-tag>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="StraighthairShipmentTracking"
          :params="{
            projectId: query.projectId,
            productType: query.productType,
            startDate: query.startDate,
            endDate: query.endDate,
          }"
          size="mini"
          type="warning"
        />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { watch, nextTick, inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import moment from 'moment'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { installProjectTypeEnum } from '@enum-ms/project'

const commonParams = inject('commonParams')

watch(
  commonParams,
  (data = {}) => {
    nextTick(() => {
      for (const key in data) {
        crud.query[key] = data[key]
      }
      crud.toQuery()
    })
  },
  { immediate: true, deep: true }
)

const times = PICKER_OPTIONS_SHORTCUTS[1]?.value()

const defaultQuery = {
  startDate: moment(times[0]).valueOf(),
  endDate: moment(times[1]).valueOf(),
  serialNumber: undefined,
  specification: undefined,
  material: undefined,
  productType: installProjectTypeEnum.ARTIFACT.V,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>

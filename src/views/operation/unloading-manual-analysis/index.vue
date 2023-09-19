<template>
  <div class="app-container">
    <div class="head-container">
      <crudOperation>
        <template #optLeft>
          <el-date-picker
            v-model="year"
            type="year"
            size="small"
            style="width: 100px"
            placeholder="选择年"
            class="filter-item"
            value-format="x"
            clearable
            :disabled-date="disabledDate"
            @change="crud.toQuery"
          />
          <!-- <workshop-select
            v-model="workshopId"
            placeholder="请选择车间"
            :factory-id="factoryId"
            style="width: 200px"
            class="filter-item"
            clearable
            @change="crud.toQuery"
          /> -->
        </template>
        <template #viewLeft>
          <export-button class="filter-item"> 下料人工分析清单 </export-button>
          <el-tag size="medium" style="align-self: center">单位：元/吨</el-tag>
        </template>
      </crudOperation>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column v-if="columns.visible('month')" label="月份" prop="month" align="center" />
      <el-table-column v-if="columns.visible('personNumber')" label="人员数量" prop="personNumber" align="center" />
      <el-table-column v-if="columns.visible('Attendance')" label="出勤（工日）" prop="Attendance" align="center" />
      <el-table-column v-if="columns.visible('workQuantity')" label="工程量（t）" prop="workQuantity" align="center" />
      <el-table-column
        v-if="columns.visible('perCapitaProduction')"
        label="人均产量（吨/工日）"
        prop="perCapitaProduction"
        align="center"
      />
      <el-table-column v-if="columns.visible('totalAmount')" label="应付工程款总额（元）" prop="totalAmount" align="center" />
      <el-table-column
        v-if="columns.visible('unloadingAveragePrice')"
        label="下料均价（元/吨）"
        prop="unloadingAveragePrice"
        align="center"
      />
      <!-- <el-table-column v-if="columns.visible('remark')" label="备注" prop="remark" align="center" /> -->
    </common-table>
  </div>
</template>

<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import moment from 'moment'
import { tableSummary } from '@/utils/el-extra'
import crudOperation from '@crud/CRUD.operation'
// import workshopSelect from '@/components-system/base/workshop-select.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(moment().valueOf().toString())
// const workshopId = ref()
// const factoryId = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '下料人工分析',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

function disabledDate(time) {
  return time > new Date()
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}
</script>
<style lang="scss" scoped>
</style>

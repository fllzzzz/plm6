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
          <export-button class="filter-item"> 构件分析清单 </export-button>
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
      <el-table-column v-if="columns.visible('month')" label="月份" prop="month" align="center" width="60px" fixed="left" />
      <el-table-column
        v-if="columns.visible('monthProduction')"
        label="月产（吨）"
        prop="monthProduction"
        align="center"
        width="100px"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('personNumber')"
        label="人员数量"
        prop="personNumber"
        align="center"
        width="100px"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('Attendance')"
        label="出勤（工日）"
        prop="Attendance"
        align="center"
        width="100px"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('productionAmount')"
        label="生产量（吨）"
        prop="productionAmount"
        align="center"
        min-width="200px"
      >
        <el-table-column v-if="columns.visible('productionAmount')" label="焊接H型钢" prop="productionAmount" align="center" width="300px">
        </el-table-column>
        <el-table-column v-if="columns.visible('other')" label="其他" prop="other" align="center" width="300px"> </el-table-column>
        <el-table-column v-if="columns.visible('subtotal')" label="小计" prop="subtotal" align="center" width="300px"> </el-table-column>
      </el-table-column>
      <el-table-column min-width="1px" />
      <el-table-column
        v-if="columns.visible('perCapitaProduction')"
        label="人均产量（吨/月）"
        prop="perCapitaProduction"
        align="center"
        width="140px"
        fixed="right"
      />
      <el-table-column
        v-if="columns.visible('totalLaborCost')"
        label="人工费总额（元）"
        prop="totalLaborCost"
        align="center"
        width="140px"
        fixed="right"
      />
      <el-table-column
        v-if="columns.visible('processUnitPrice')"
        label="工序单价（元/吨）"
        prop="processUnitPrice"
        align="center"
        width="140px"
        fixed="right"
      />
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
    title: '构件分析',
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

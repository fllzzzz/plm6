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
          <project-cascader v-model="projectId" class="filter-item" @change="crud.toQuery" />
        </template>
        <template #viewLeft>
          <export-button class="filter-item"> 检测费清单 </export-button>
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
      <el-table-column type="index" label="序号" prop="indx" align="center" />
      <el-table-column v-if="columns.visible('projectName')" label="项目" prop="projectName" align="center" min-width="160px" />
      <el-table-column v-if="columns.visible('production')" label="产量（吨）" prop="production" align="center" />
      <el-table-column v-if="columns.visible('rawMaterialReinspection')" label="原材料复检" prop="rawMaterialReinspection" align="center" />
      <el-table-column v-if="columns.visible('weldTestPlates')" label="焊接试板" prop="weldTestPlates" align="center" />
      <el-table-column v-if="columns.visible('antiSlipTest')" label="抗滑移试验" prop="antiSlipTest" align="center" />
      <el-table-column v-if="columns.visible('specimenProcessing')" label="试件加工" prop="specimenProcessing" align="center" />
      <el-table-column v-if="columns.visible('weldFlawDetection')" label="焊缝探伤" prop="weldFlawDetection" align="center" />
      <el-table-column v-if="columns.visible('totalAmount')" label="合计" prop="totalAmount" align="center" />
      <el-table-column v-if="columns.visible('averageTestingFee')" label="平均检测费（元/吨）" prop="averageTestingFee" align="center" />
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
import projectCascader from '@comp-base/project-cascader'
import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(moment().valueOf().toString())
const projectId = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '检测费',
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

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

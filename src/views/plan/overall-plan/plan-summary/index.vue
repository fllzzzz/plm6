<template>
  <div class="app-container">
      <!--工具栏-->
      <div class="head-container">
        <mHeader />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        show-summary
        :stripe="false"
        :summary-method="getSummaries"
        :max-height="maxHeight"
        return-source-data
        :showEmptySymbol="false"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('projectName')" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目" min-width="150">
          <template v-slot="scope">
            <el-tooltip :content="scope.row.project.serialNumber+' '+scope.row.project.name" :disabled="!scope.row.project" :show-after="50" placement="top">
              <span>{{scope.row.projectName}}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('monomerName')" key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="150" />
        <el-table-column align="center" label="计划交期">
          <el-table-column key="month1" prop="month1" align="center" label="1月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month1)? scope.row.month1.toFixed(2): '-'}}</div>
            </template>
          </el-table-column>
          <el-table-column key="month2" prop="month2" align="center" label="2月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month2)? scope.row.month2.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month3" prop="month3" align="center" label="3月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month3)? scope.row.month3.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month4" prop="month4" align="center" label="4月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month4)? scope.row.month4.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month5" prop="month5" align="center" label="5月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month5)? scope.row.month5.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month6" prop="month6" align="center" label="6月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month6)? scope.row.month6.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month7" prop="month7" align="center" label="7月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month7)? scope.row.month7.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month8" prop="month8" align="center" label="8月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month8)? scope.row.month8.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month9" prop="month9" align="center" label="9月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month9)? scope.row.month9.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month10" prop="month10" align="center" label="10月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month10)? scope.row.month10.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month11" prop="month11" align="center" label="11月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month11)? scope.row.month11.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="month12" prop="month12" align="center" label="12月">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.month12)? scope.row.month12.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="monthSummary" prop="monthSummary" align="center" label="合计">
            <template v-slot="scope">
              <div>{{ isNotBlank(scope.row.monthSummary)? scope.row.monthSummary.toFixed(2): '-' }}</div>
            </template>
          </el-table-column>
        </el-table-column>
      </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-summary'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import { isNotBlank } from '@data-type/index'
import { planSummaryListPM as permission } from '@/page-permission/plan'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '排产汇总',
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['type', 'year'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-summary',
  paginate: true,
  extraHeight: 40
})

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  const property = ['month1', 'month2', 'month3', 'month4', 'month5', 'month6', 'month7', 'month8', 'month9', 'month10', 'month11', 'month12', 'monthSummary']
  const sumData = {}
  columns.forEach((column, index) => {
    if (index === 1) {
      sums[index] = '计划完成合计'
      return
    }
    if (!property.includes(column.property)) return
    const values = data.map(item => Number(item[column.property]))
    if (!values.every(value => isNaN(value))) {
      sums[index] = values.reduce((prev, curr) => {
        const value = Number(curr)
        if (!isNaN(value)) {
          return prev + curr
        } else {
          return prev
        }
      }, 0)
      sumData[column.property] = sums[index]
    }
    sums[index] = Number(sums[index]).toFixed(2)
  })
  return sums
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    v.projectName = v.project && v.project.shortName ? v.project.serialNumber + ' ' + v.project.shortName : '-'
    v.monomerName = v.monomer && v.monomer.name ? v.monomer.name : '-'
    if (v.details && v.details.length > 0) {
      v.monthSummary = 0
      v.details.map(k => {
        v['month' + Number(k.month)] = k.mete
        v.monthSummary = k.mete ? v.monthSummary + k.mete : v.monthSummary
      })
    }
    return v
  })
}
</script>
<style lang="scss" scoped>
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
.progress-left {
  font-size: 12px;
  position: absolute;
  z-index: 200;
  top: 50%;
  transform: translateY(-50%);
  left: 0;
}
.progress-right {
  font-size: 12px;
  position: absolute;
  z-index: 200;
  top: 50%;
  transform: translateY(-50%);
  right: 50px;
}
</style>

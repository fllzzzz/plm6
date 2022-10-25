<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!-- 表格 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('scheduleTime')"
        align="center"
        key="scheduleTime"
        prop="scheduleTime"
        :show-overflow-tooltip="true"
        label="排产日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.scheduleTime ? parseTime(scope.row.scheduleTime, '{y}/{m}/{d}') : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('OrderNumber')"
        align="center"
        key="OrderNumber"
        prop="OrderNumber"
        :show-overflow-tooltip="true"
        label="切割指令号"
      >
        <template v-slot="scope">
          <table-cell-tag :name="boolPrintedEnum.V[!!scope.row.printQuantity].L" :offset="15" />
          <span>{{ scope.row.OrderNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        align="center"
        key="userName"
        prop="userName"
        :show-overflow-tooltip="true"
        label="排产人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.userName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialTypeName')"
        align="center"
        key="materialTypeName"
        prop="materialTypeName"
        :show-overflow-tooltip="true"
        label="下料方式"
      >
        <template v-slot="scope">
          <span>{{ scope.row.materialTypeName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('workshopName')"
        align="center"
        key="workshopName"
        prop="workshopName"
        :show-overflow-tooltip="true"
        label="车间"
      >
        <template v-slot="scope">
          <span>{{ scope.row.workshopName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLineName')"
        align="center"
        key="productionLineName"
        prop="productionLineName"
        :show-overflow-tooltip="true"
        label="生产线"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLineName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        align="center"
        key="taskQuantity"
        prop="taskQuantity"
        :show-overflow-tooltip="true"
        label="任务量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.taskQuantity }}/{{ scope.row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeTime')"
        align="center"
        key="completeTime"
        prop="completeTime"
        :show-overflow-tooltip="true"
        label="完成日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}/{m}/{d}') : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
          <common-button type="info" size="mini" @click="printDetail(scope.row)">打印</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/work-order-manage/machine-part.js'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import { parseTime } from '@/utils/date'
import { boolPrintedEnum } from '@enum-ms/common'
import mHeader from './module/header.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '零件工单',
    sort: [],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 查看
function views(row) {
  console.log(row, 'row')
}

// 打印
function printDetail(row) {
  console.log(row, 'row')
}
</script>
<style lang="scss" scoped>
</style>


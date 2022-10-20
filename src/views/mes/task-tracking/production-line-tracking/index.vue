<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="productionLineData"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('productionLine')"
        align="center"
        key="productionLine"
        prop="productionLine"
        :show-overflow-tooltip="true"
        label="产线"
      >
        <template v-slot="scope">
          <span>{{ scope.row.workshopInf }}>{{ scope.row.productionLine }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('type')" align="center" key="type" prop="type" :show-overflow-tooltip="true" label="类别">
        <template v-slot="scope">
          <el-tag effect="plain" :type="componentTypeEnum.V[scope.row.type].T">{{ componentTypeEnum.VL[scope.row.type] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('type')"
        align="center"
        key="type"
        prop="type"
        :show-overflow-tooltip="true"
        label="查询日期"
        min-width="120px"
      >
        <template v-slot="scope">
          <span
            >{{ scope.row.startDate ? parseTime(scope.row.startDate, '{y}/{m}/{d}') : '-' }} ~
            {{ scope.row.endDate ? parseTime(scope.row.endDate, '{y}/{m}/{d}') : '-' }}</span
          >
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('type')"
        align="center"
        key="type"
        prop="type"
        :show-overflow-tooltip="true"
        label="任务量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalQuantity }}/{{ scope.row.totalWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('type')"
        align="center"
        key="type"
        prop="type"
        :show-overflow-tooltip="true"
        label="完成量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.actualQuantity }}/{{ scope.row.actualWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('type')"
        align="center"
        key="type"
        prop="type"
        :show-overflow-tooltip="true"
        label="完成率"
        min-width="180px"
      >
        <template v-slot="scope">
          <el-progress :text-inside="true" stroke-linecap="square" :stroke-width="22" :percentage="scope.row.complete" status="success" />
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 产线跟踪详情 -->
    <production-line-tracking-detail v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import { componentTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header.vue'
import productionLineTrackingDetail from './production-line-tracking-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productionLineData = [
  {
    productionLine: '一线',
    workshopInf: '一车间',
    type: 2,
    startDate: '1650000000',
    endDate: '1670000000',
    totalQuantity: 50,
    totalWeight: 1000,
    actualQuantity: 60,
    actualWeight: 1000,
    complete: 60,
    status: '正常'
  },
  {
    productionLine: '二线',
    workshopInf: '一车间',
    type: 1,
    startDate: '1650000000',
    endDate: '1670000000',
    totalQuantity: 50,
    totalWeight: 1000,
    actualQuantity: 60,
    actualWeight: 1000,
    complete: 30,
    status: '滞后'
  }
]
const tableRef = ref()
const drawerVisible = ref(false)
const detailData = ref({})
const { crud, columns } = useCRUD(
  {
    title: '产线跟踪',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function views(row) {
  drawerVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
</style>

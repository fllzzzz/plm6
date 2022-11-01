<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
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
        v-if="columns.visible('name')"
        align="center"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="产线"
      >
        <template v-slot="scope">
          <span>{{scope.row.workShopName}}>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('taskType')" align="center" key="taskType" prop="taskType" :show-overflow-tooltip="true" label="类别">
        <template v-slot="scope">
          <el-tag effect="plain" :type="componentTypeTag[componentTypeEnum.VK[scope.row.taskType]]">{{ componentTypeEnum.VL[scope.row.taskType] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('startDate')"
        align="center"
        key="startDate"
        prop="startDate"
        :show-overflow-tooltip="true"
        label="查询日期"
        min-width="120px"
      >
        <template v-slot="scope">
          <span style="color: blue">{{ scope.row.startDate ? parseTime(scope.row.startDate, '{y}-{m}-{d}') : '-' }} ~
            {{ scope.row.endDate ? parseTime(scope.row.endDate, '{y}-{m}-{d}') : '-' }}</span
          >
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
        v-if="columns.visible('completeQuantity')"
        align="center"
        key="completeQuantity"
        prop="completeQuantity"
        :show-overflow-tooltip="true"
        label="完成量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }}/{{ scope.row.completeMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('rate')"
        align="center"
        key="rate"
        prop="rate"
        :show-overflow-tooltip="true"
        label="完成率"
        min-width="180px"
      >
        <template v-slot="scope">
          <el-progress :text-inside="true" stroke-linecap="square" :stroke-width="22" :percentage="scope.row.rate" status="success" />
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <!-- <pagination /> -->
    <!-- 产线跟踪详情 -->
    <production-line-tracking-detail v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/task-tracking/production-line-tracking.js'
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import { componentTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
// import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import productionLineTrackingDetail from './production-line-tracking-detail/index.vue'

// 由于mes枚举构件、部件的type值相同，单独定义枚举type值
const componentTypeTag = {
  [componentTypeEnum.ARTIFACT.K]: 'success',
  [componentTypeEnum.ASSEMBLE.K]: 'warning',
  [componentTypeEnum.MACHINE_PART.K]: ''
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const drawerVisible = ref(false)
const detailData = ref({})
const { crud, CRUD, columns } = useCRUD(
  {
    title: '产线跟踪',
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

function views(row) {
  drawerVisible.value = true
  detailData.value = row
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  const _content = []
  for (const key in data) {
    const productType = key === 'artifactList' ? componentTypeEnum.ARTIFACT.V : (key === 'assembleList' ? componentTypeEnum.ASSEMBLE.V : componentTypeEnum.MACHINE_PART.V)
    data[key].map(v => {
      v.productType = productType
      _content.push(v)
    })
  }
  data.content = _content
}
</script>
<style lang="scss" scoped>
</style>

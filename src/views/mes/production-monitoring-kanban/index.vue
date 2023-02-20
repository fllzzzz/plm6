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
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目" width="200px">
        <template v-slot="scope">
          <span>{{ scope.row.project?.contractNo }}-{{ scope.row.project?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        align="center"
        :show-overflow-tooltip="true"
        label="单体"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomer?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('area.name')"
        key="area.name"
        prop="area.name"
        align="center"
        :show-overflow-tooltip="true"
        label="区域"
      >
        <template v-slot="scope">
          <span>{{ scope.row.area?.name }}</span>
        </template>
      </el-table-column>

      <!-- <el-table-column
        v-if="columns.visible('startDate')"
        align="center"
        key="startDate"
        prop="startDate"
        :show-overflow-tooltip="true"
        label="查询日期"
        min-width="120px"
      >
        <template v-slot="scope">
          <span style="color: blue">
            {{ scope.row.startDate ? parseTime(scope.row.startDate, '{y}-{m}-{d}') : '-' }} ~
            {{ scope.row.endDate ? parseTime(scope.row.endDate, '{y}-{m}-{d}') : '-' }}
          </span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        align="center"
        key="taskQuantity"
        prop="taskQuantity"
        :show-overflow-tooltip="true"
        label="排产量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.taskQuantity }}/{{ scope.row.taskNetWeight?.toFixed(DP.COM_WT__KG) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeQuantity')"
        align="center"
        key="completeQuantity"
        prop="completeQuantity"
        :show-overflow-tooltip="true"
        label="实际完成量（件/kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }}/{{ scope.row.completeNetWeight?.toFixed(DP.COM_WT__KG) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('rate')"
        align="center"
        key="rate"
        prop="rate"
        :show-overflow-tooltip="true"
        label="达成率"
        min-width="180px"
      >
        <template v-slot="scope">
          <el-progress
            :text-inside="true"
            stroke-linecap="square"
            :stroke-width="22"
            :percentage="((scope.row.completeQuantity / scope.row.taskQuantity) * 100).toFixed(2)"
            status="success"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('status')"
        align="center"
        key="status"
        prop="status"
        :show-overflow-tooltip="true"
        label="状态"
        width="80px"
      >
        <template v-slot="scope">
          <el-tag effect="plain" :type="componentTypeTag[componentTypeEnum.VK[scope.row.taskType]]">{{
            componentTypeEnum.VL[scope.row.taskType]
          }}</el-tag>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <!-- <pagination /> -->
    <!-- 产线跟踪详情 -->
    <!-- <production-line-tracking-detail v-model:visible="drawerVisible" :detail-data="detailData" /> -->
  </div>
</template>
<script setup>
import { ref, reactive, provide, watch } from 'vue'
import crudApi, { getSummary } from '@/api/mes/production-monitoring-kanban/kanban.js'
import useCRUD from '@compos/use-crud'
import { DP } from '@/settings/config'
// import { parseTime } from '@/utils/date'
import { componentTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
// import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
// import productionLineTrackingDetail from './production-line-tracking-detail/index.vue'

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
    title: '生产监控看板',
    sort: [],
    optShow: { ...optShow },
    // permission: { ... permission },
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

// 项目汇总数据（子页面使用）
const projectInfo = reactive({
  summary: {}, // 项目汇总数量
  loading: true
})

provide('projectInfo', projectInfo)

watch(
  () => crud.query.workshopId,
  (val) => {
    fetchProjectInfo()
    crud.toQuery()
  },
  { deep: true }
)

// 获取项目汇总数据
async function fetchProjectInfo() {
  projectInfo.loading = true
  try {
    const res = (await getSummary(crud.query.workshopId)) || {}
    projectInfo.summary = res
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {}
</script>
<style lang="scss" scoped>
</style>

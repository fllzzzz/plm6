<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div>
      <common-button v-permission="permission.status" class="btn" size="mini" type="warning" style="float: right; margin-bottom: 8px" @click.stop="groupsDetail">
        班组状态
      </common-button>
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
          v-if="columns.visible('project')"
          key="project"
          prop="project"
          :show-overflow-tooltip="true"
          label="项目"
          width="200px"
        >
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
          label="排产量（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}/{{ (scope.row.taskNetWeight / 1000)?.toFixed(2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeQuantity')"
          align="center"
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          label="实际完成量（件/吨）"
        >
          <template v-slot="scope">
            <span
@click.stop="views(scope.row)"
style="color: #409eff; cursor: pointer"
              >{{ scope.row.completeQuantity }}/{{ (scope.row.completeNetWeight / 1000)?.toFixed(2) }}</span
            >
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
            <el-tag effect="plain" :type="productionKanbanTypeEnum.V[scope.row.status].T">{{
              productionKanbanTypeEnum.VL[scope.row.status]
            }}</el-tag>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
      <pagination />
    </div>
    <!-- 生产监控看板详情 -->
    <kanban-detail v-model:visible="detailDialogVisible" :detail-list="detailList" :workshopId="crud.query.workshopId" />
    <!-- 班组状态详情 -->
    <group-status-drawer v-model:visible="drawerVisible" :workshopId="crud.query.workshopId" :group-detail-data="crud.data[0]" />
  </div>
</template>
<script setup>
import { ref, reactive, provide, watch } from 'vue'
import crudApi, { getSummary } from '@/api/mes/production-monitoring-kanban/kanban.js'
import useCRUD from '@compos/use-crud'
import { productionKanbanTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import { productionMonitoringKanbanPM as permission } from '@/page-permission/mes'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import kanbanDetail from './module/kanban-detail.vue'
import groupStatusDrawer from './module/group-status-drawer.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailDialogVisible = ref(false)
const drawerVisible = ref(false)
const detailList = ref({})
// const groupDetailData = ref([])
const { crud, CRUD, columns } = useCRUD(
  {
    title: '生产监控看板',
    sort: [],
    optShow: { ...optShow },
    permission: { ... permission },
    crudApi: { ...crudApi },
    requiredQuery: ['workshopId'],
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.btn'],
  paginate: true
})

function views(row) {
  detailDialogVisible.value = true
  detailList.value = row
}

// 项目汇总数据（子页面使用）
const projectInfo = reactive({
  summary: {}, // 项目汇总数量
  loading: true
})

provide('projectInfo', projectInfo)
provide('permission', permission)

watch(
  () => crud.query.workshopId,
  (val) => {
    if (val) {
      fetchProjectInfo()
      crud.toQuery()
    }
  },
  { immediate: true }
)

// 班组详情
function groupsDetail() {
  drawerVisible.value = true
}

// 获取项目汇总数据
async function fetchProjectInfo() {
  projectInfo.loading = true
  try {
    const res = (await getSummary({ workshopId: crud.query.workshopId })) || {}
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

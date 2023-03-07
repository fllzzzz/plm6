<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading || !loaded"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        width="200px"
        fixed="left"
      >
        <template v-slot="scope">
          <span>{{ scope.row.project?.serialNumber }}-{{ scope.row.project?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unit" key="unit" label="单位" align="center" fixed="left" />
      <el-table-column prop="rawNetWeight" key="rawNetWeight" label="原材料出库量" align="center" fixed="left" width="140">
        <template #default="{ row }">
          <span @click.stop="getOutBound(row)" style="cursor: pointer" class="tc-danger">{{ row.rawNetWeight?.toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="taskNetWeight" key="taskNetWeight" label="任务量" align="center" fixed="left" width="140">
        <template #default="{ row }">
          <span @click.stop="getTaskDetail(row)" style="cursor: pointer" class="tc-danger">{{ row.taskNetWeight }}</span>
        </template>
      </el-table-column>
      <template v-for="item in process" :key="item.id">
        <el-table-column
          v-if="(item.productType & componentTypeEnum.ARTIFACT.V) | (item.productType & componentTypeEnum.ASSEMBLE.V) && (item.productionLineTypeEnum & artifactProductLineEnum.TRADITION.V)"
          :label="item.name"
          align="center"
          width="110px"
        >
          <template #default="{ row }">
            <!-- <el-tooltip v-if="row.processMap[item.id]" content="不合格数 / 检验数" placement="top"> -->
            <span style="cursor: pointer" @click.stop="getProcessDetail(row, item)" v-if="row.processMap[item.id]">
              <span class="tc-danger">{{ row.processMap[item.id]?.unQuantity }}</span>
              <span> / </span>
              <span class="tc-danger">{{ row.processMap[item.id]?.unNetWeight }}</span>
            </span>
            <!-- </el-tooltip> -->
            <span v-else> / </span>
          </template>
        </el-table-column>
      </template>
      <el-table-column prop="totalNetWeight" key="totalNetWeight" label="合计" align="center" width="140" fixed="right" />
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 出库记录详情 -->
    <out-bound-detail v-model:visible="drawerVisible" :info="info" />
    <!-- 排产记录 -->
    <task-detail v-model:visible="taskDrawerVisible" :task-info="taskInfo" />
    <!-- 工序在制品统计 -->
    <process-detail v-model:visible="processDrawerVisible" :process-info="processInfo" :process-data="processData" />
  </div>
</template>
<script setup>
import { ref, provide } from 'vue'
import crudApi from '@/api/mes/task-tracking/wip-statistics.js'
import useCRUD from '@compos/use-crud'
// import { parseTime } from '@/utils/date'
import { mesWipStatisticsPM as permission } from '@/page-permission/mes'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import useProcess from '@compos/store/use-process'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import outBoundDetail from './module/out-bound-detail.vue'
import taskDetail from './module/task-detail.vue'
import processDetail from './module/process-detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const drawerVisible = ref(false)
const info = ref({})
const taskDrawerVisible = ref(false)
const taskInfo = ref({})
const processDrawerVisible = ref(false)
const processInfo = ref({})
const processData = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '在制品统计',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

provide('permission', permission)
const { loaded, process } = useProcess()

// 出库记录
function getOutBound(row) {
  drawerVisible.value = true
  info.value = row
}

// 排产记录
function getTaskDetail(row) {
  taskDrawerVisible.value = true
  taskInfo.value = row
}

// 工序在制品统计
function getProcessDetail(row, item) {
  processDrawerVisible.value = true
  processInfo.value = item
  processData.value = row
}
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.processMap = {}
    v.processDTOList.forEach((p) => {
      v.processMap[p.id] = p
    })
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>

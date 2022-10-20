<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader @change="handleProductType" />
    </div>
    <div style="display: flex;justify-content: space-between">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="workOderData"
        highlight-current-row
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        row-key="projectId"
        style="width: 100%; flex: 4;"
        @current-change="currentChange"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="排产工单号"
        >
          <template v-slot="scope">
            <span>{{ scope.row.schedulingOrderNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName') && transformTab !== processMaterialListTypeEnum.MACHINE_PART.V"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="所属项目"
        >
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('endDate')"
          align="center"
          key="endDate"
          prop="endDate"
          :show-overflow-tooltip="true"
          label="计划完成日期"
        >
          <template v-slot="scope">
            <span>{{ scope.row.endDate ? parseTime(scope.row.endDate, '{y}/{m}/{d}') : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="总量（件/kg）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="实际完成"
        >
          <template v-slot="scope">
            <span>{{ scope.row.actualQuantity }}/{{ scope.row.actualWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="完成率"
        >
          <template v-slot="scope">
            <span>{{ scope.row.completeRate }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="排产人"
        >
          <template v-slot="scope">
            <span>{{ scope.row.schedulingName }}</span>
          </template>
        </el-table-column>
      </common-table>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <process-detail :process-list="processList" :transform-tab="transformTab" style="flex: 2"/>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
// import crudApi from ''
import { parseTime } from '@/utils/date'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header.vue'
import processDetail from './process-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const workOderData = [
  { schedulingOrderNumber: '构-2022/10/13-01', projectName: '111-项目', endDate: 1670000000, quantity: 20, weight: 1000, actualQuantity: 10, actualWeight: 800, completeRate: 85, schedulingName: '超级管理员' },
  { schedulingOrderNumber: '构-2022/10/13-02', projectName: '222-项目', endDate: 1670000000, quantity: 50, weight: 1000, actualQuantity: 30, actualWeight: 600, completeRate: 65, schedulingName: '超级管理员' }
]
const tableRef = ref()
const transformTab = ref()
const processList = ref({})
const { crud, CRUD, columns } = useCRUD(
  {
    title: '工单跟踪',
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

// 切换构件、零件
function handleProductType(val) {
  transformTab.value = val
  if (transformTab.value === processMaterialListTypeEnum.MACHINE_PART.V) {
    console.log('零件')
    // crud.crudApi = { ...partProcess }
  } else {
    console.log('构件')
    // crud.crudApi = { ...crudApi }
  }
}

function currentChange(row) {
  console.log(row, 'row')
  processList.value = row
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    return v
  })
}
</script>

<style>
</style>

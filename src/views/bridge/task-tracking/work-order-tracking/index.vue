<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="width: 50%">
        <div class="head-container">
          <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          highlight-current-row
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          style="width: 100%"
          @current-change="currentChange"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column
            v-if="columns.visible('orderNumber')"
            align="center"
            key="orderNumber"
            prop="orderNumber"
            :show-overflow-tooltip="true"
            label="排产工单号"
            min-width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.orderNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('project') && productType !== bridgeComponentTypeEnum.MACHINE_PART.V"
            key="project.name"
            prop="project"
            :show-overflow-tooltip="true"
            label="所属项目"
            min-width="100px"
          >
            <template v-slot="scope">
              <span>{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeTime')"
            align="center"
            key="completeTime"
            prop="completeTime"
            :show-overflow-tooltip="true"
            label="计划完成日期"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('totalQuantity')"
            align="center"
            key="totalQuantity"
            prop="totalQuantity"
            :show-overflow-tooltip="true"
            label="总量（件/kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.totalQuantity }}/{{ scope.row.totalMete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeQuantity')"
            align="center"
            key="completeQuantity"
            prop="completeQuantity"
            :show-overflow-tooltip="true"
            label="实际完成（件/kg）"
            width="130px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeQuantity }}/{{ scope.row.completeMete.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('rate')"
            align="center"
            key="rate"
            prop="rate"
            :show-overflow-tooltip="true"
            label="完成率"
          >
            <template v-slot="scope">
              <span>{{ ((scope.row.completeQuantity / scope.row.totalQuantity) * 100).toFixed(2) }}%</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('userName') && productType === bridgeComponentTypeEnum.MACHINE_PART.V"
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
        </common-table>
        <!-- 分页 -->
        <pagination />
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div>
      <div style="width: 48%">
        <process-detail :process-list="processList" />
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, provide, computed, watch } from 'vue'
import { get, machinePart } from '@/api/bridge/bridge-task-tracking/work-order-tracking.js'
import { parseTime } from '@/utils/date'
import { projectNameFormatter } from '@/utils/project'
import { bridgeWorkOrderTrackingPM as permission } from '@/page-permission/bridge'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import processDetail from './process-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const processList = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '工单跟踪',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    requiredQuery: ['productType'],
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)
const productType = computed(() => {
  return crud.query.productType
})

provide('productType', productType)

watch(
  () => productType.value,
  (val) => {
    if (val) {
      processList.value = {}
      crud.query.workshopId = undefined
      crud.query.productionLineId = undefined
    }
  },
  { immediate: true, deep: true }
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function currentChange(row) {
  processList.value = row
}

CRUD.HOOK.beforeToQuery = () => {
  crud.crudApi.get = crud.query.productType === bridgeComponentTypeEnum.BOX.V ? get : machinePart
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    return v
  })
}
</script>

<style>
</style>

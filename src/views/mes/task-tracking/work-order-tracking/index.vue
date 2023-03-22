<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="width: 60%">
        <div class="head-container">
          <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          highlight-current-row
          :empty-text="crud.emptyText"
          :show-empty-symbol="false"
          :max-height="maxHeight"
          style="width: 100%"
          @current-change="currentChange"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
          <el-table-column
            v-if="columns.visible('orderNumber')"
            align="center"
            key="orderNumber"
            prop="orderNumber"
            :show-overflow-tooltip="true"
            label="排产工单号"
            min-width="140px"
            fixed="left"
          >
            <template v-slot="scope">
              <span>{{ scope.row.orderNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('project') && productType !== componentTypeEnum.MACHINE_PART.V"
            key="project.name"
            prop="project"
            :show-overflow-tooltip="true"
            label="所属项目"
            min-width="120px"
            fixed="left"
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
            fixed="left"
            width="130px"
          >
            <template v-slot="scope">
              <span v-if="scope.row.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V">{{
                scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-'
              }}</span>
              <span v-else>-</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('totalQuantity')"
            align="center"
            key="totalQuantity"
            prop="totalQuantity"
            :show-overflow-tooltip="true"
            label="总数量（件）"
            width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.totalQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('totalNetWeight')"
            align="center"
            key="totalNetWeight"
            prop="totalNetWeight"
            :show-overflow-tooltip="true"
            label="总净重（kg）"
            width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.totalNetWeight?.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('totalGrossWeight')"
            align="center"
            key="totalGrossWeight"
            prop="totalGrossWeight"
            :show-overflow-tooltip="true"
            label="总毛重（kg）"
            width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.totalGrossWeight?.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeQuantity')"
            align="center"
            key="completeQuantity"
            prop="completeQuantity"
            :show-overflow-tooltip="true"
            label="完成数量（件）"
            width="110px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeNetWeight')"
            align="center"
            key="completeNetWeight"
            prop="completeNetWeight"
            :show-overflow-tooltip="true"
            label="完成总净重（kg）"
            width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeNetWeight?.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeGrossWeight')"
            align="center"
            key="completeGrossWeight"
            prop="completeGrossWeight"
            :show-overflow-tooltip="true"
            label="完成总毛重（kg）"
            width="120px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeGrossWeight?.toFixed(DP.COM_WT__KG) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('rate')"
            align="center"
            key="rate"
            prop="rate"
            :show-overflow-tooltip="true"
            label="完成率"
             fixed="right"
          >
            <template v-slot="scope">
              <span>{{ ((scope.row.completeQuantity / scope.row.totalQuantity) * 100).toFixed(2) }}%</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('userName') && productType === componentTypeEnum.MACHINE_PART.V"
            align="center"
            key="userName"
            prop="userName"
            :show-overflow-tooltip="true"
            label="排产人"
            fixed="right"
            width="100px"
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
      <div style="width: 38%">
        <process-detail :process-list="processList" />
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, provide, computed, watch } from 'vue'
import { get, machinePart } from '@/api/mes/task-tracking/work-order-tracking.js'
import { parseTime } from '@/utils/date'
import { projectNameFormatter } from '@/utils/project'
import { mesWorkOrderTrackingPM as permission } from '@/page-permission/mes'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
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
    invisibleColumns: ['totalGrossWeight', 'completeGrossWeight'],
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
  crud.crudApi.get = crud.query.productType === componentTypeEnum.ARTIFACT.V ? get : machinePart
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    return v
  })
}
</script>

<style>
</style>

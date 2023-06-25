<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader ref="headerRef">
        <template #viewLeft>
          <!-- <print-table
            v-permission="permission.print"
            api-key="mesProductionStatisticsReport"
            :params="{
              status: crud.query.status,
              startDate: crud.query.startDate,
              endDate: crud.query.endDate,
              weightStatus: crud.query.weightStatus,
            }"
            size="mini"
            type="warning"
            class="print-table"
          /> -->
        </template>
      </mHeader>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading || !loaded"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      :show-empty-symbol="false"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60px" fixed="left" />
      <el-table-column
        v-if="columns.visible('project')"
        :show-overflow-tooltip="true"
        prop="project"
        label="项目"
        header-align="center"
        width="160px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ row.project ? row.project?.serialNumber + '-' + row.project?.name : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unit')" prop="unit" label="单位" align="center" fixed="left" />
      <el-table-column
        v-if="columns.visible('rawTotalNetWeight')"
        :show-overflow-tooltip="true"
        prop="rawTotalNetWeight"
        label="原材料累计出库"
        align="center"
        width="130px"
        fixed="left"
      >
        <template #default="{ row }">
          <span style="cursor: pointer" class="tc-primary" @click.stop="showTotal(row)">{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? row.rawTotalNetWeight : row.rawTotalGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('rawNetWeight')"
        :show-overflow-tooltip="true"
        prop="rawNetWeight"
        label="原材料本月出库"
        align="center"
        width="130px"
        fixed="left"
      >
        <template #default="{ row }">
          <span style="cursor: pointer" class="tc-primary" @click.stop="showMonth(row)">{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? row.rawNetWeight : row.rawGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskNetWeight')"
        prop="taskNetWeight"
        label="任务量"
        align="center"
        :show-overflow-tooltip="true"
        fixed="left"
      >
        <template #default="{ row }">
          <span style="cursor: pointer" class="tc-primary" @click.stop="showTask(row)">{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? row.taskNetWeight : row.taskGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unit')" prop="unit" label="在制品" align="center">
        <template v-for="item in processSortList" :key="item.id">
          <el-table-column
            v-if="
              item.productType &&
              componentTypeEnum.ARTIFACT.V | item.productType &&
              componentTypeEnum.ASSEMBLE.V &&
              item.productionLineTypeEnum & artifactProductLineEnum.TRADITION.V &&
              item.productType !== componentTypeEnum.ENCLOSURE.V
            "
            :label="item.name"
            align="center"
            width="110px"
          >
            <template #default="{ row }">
              <!-- <div
                v-if="row.processMap[item.id] && row.processMap[item.id]?.inspectionQuantity === row.processMap[item.id]?.quantity"
                style="color: #13ce66"
              >
                √
              </div> -->
              <div>
                <div @click.stop="getProcessDetail(row, item)" style="cursor: pointer" v-if="row.processMap[item.id]">
                  <span class="tc-primary">{{ row.processMap[item.id]?.unQuantity }}</span>
                  <span> / </span>
                  <span class="tc-primary">{{
                    crud.query.weightStatus === weightTypeEnum.NET.V
                      ? row.processMap[item.id]?.unNetWeight
                      : row.processMap[item.id]?.unGrossWeight
                  }}</span>
                </div>
                <span v-else> - </span>
              </div>
            </template>
          </el-table-column>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('finishNetWeight')"
        prop="finishNetWeight"
        label="制成品"
        align="center"
        :show-overflow-tooltip="true"
        fixed="right"
      >
        <template #default="{ row }">
          <span style="cursor: pointer" class="tc-primary" @click.stop="getUps(row)">{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? row.finishNetWeight : row.finishGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        :show-overflow-tooltip="true"
        prop="totalNetWeight"
        :label="`合计\n（在制品+制成品）`"
        align="center"
        width="140px"
        fixed="right"
      >
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.totalNetWeight : row.totalGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('lossNetWeight')"
        prop="lossNetWeight"
        label="损耗量"
        align="center"
        :show-overflow-tooltip="true"
        fixed="right"
      >
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.lossNetWeight : row.lossGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('lossRate')" prop="lossRate" label="损耗率" align="center" fixed="right">
        <template #default="{ row }">
          <span>{{ row.lossRate }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 累计出库记录详情 -->
    <total-out-bound-detail v-model:visible="drawerVisible" :info="info" />
    <!-- 本月出库记录详情 -->
    <month-out-bound-detail v-model:visible="monthDrawerVisible" :info="monthInfo" :time-query="commonParams" />
    <!-- 任务量详情 -->
    <task-detail v-model:visible="taskDrawerVisible" :task-info="taskInfo" :workshopId="crud.query.workshopId" :query-date="crud.query.date" />
    <!-- 工序详情 -->
    <process-detail v-model:visible="processDrawerVisible" :process-info="processInfo" :process-data="processData" :workshopId="crud.query.workshopId" />
    <!-- 制成品详情 -->
    <ups-detail v-model:visible="upsDrawerVisible" :ups-info="upsInfo" :workshopId="crud.query.workshopId" :query-date="crud.query.date" />
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'
import crudApi from '@/api/mes/factory-report/product-statistics.js'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useProcess from '@compos/store/use-process'
import pagination from '@crud/Pagination'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { mesProductStatisticsPM as permission } from '@/page-permission/mes'
import mHeader from './module/header.vue'
import totalOutBoundDetail from './module/total-raw-material-outbound.vue'
import monthOutBoundDetail from './module/month-raw-material-outbound.vue'
import taskDetail from './module/task-detail.vue'
import processDetail from './module/process-detail.vue'
import upsDetail from './module/ups-detail.vue'

const dataFormat = ref([
  ['rawTotalNetWeight', ['to-fixed', 2]],
  ['rawTotalGrossWeight', ['to-fixed', 2]],
  ['rawNetWeight', ['to-fixed', 2]],
  ['rawGrossWeight', ['to-fixed', 2]],
  ['taskNetWeight', ['to-fixed', 2]],
  ['taskGrossWeight', ['to-fixed', 2]],
  ['finishNetWeight', ['to-fixed', 2]],
  ['finishGrossWeight', ['to-fixed', 2]],
  ['totalNetWeight', ['to-fixed', 2]],
  ['totalGrossWeight', ['to-fixed', 2]],
  ['lossNetWeight', ['to-fixed', 2]],
  ['lossGrossWeight', ['to-fixed', 2]]
])

const tableRef = ref()

const drawerVisible = ref(false)
const info = ref({})
const monthDrawerVisible = ref(false)
const monthInfo = ref({})
const taskDrawerVisible = ref(false)
const taskInfo = ref({})
const processDrawerVisible = ref(false)
const processInfo = ref({})
const processData = ref({})
const upsDrawerVisible = ref(false)
const upsInfo = ref({})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '生产统计',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { loaded, process } = useProcess()

const processSortList = computed(() => {
  const partVal = process.value.filter((v) => v.productType === componentTypeEnum.MACHINE_PART.V)
  const assembleVal = process.value.filter((o) => o.productType === componentTypeEnum.ASSEMBLE.V)
  const artifactVal = process.value.filter((m) => m.productType === componentTypeEnum.ARTIFACT.V)
  const val = [...partVal, ...assembleVal, ...artifactVal]
  return val
})

const commonParams = computed(() => {
  return {
    startDate: crud.query.startDate,
    endDate: crud.query.endDate
  }
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.processMap = {}
    v.processDTOList?.forEach((p) => {
      v.processMap[p.id] = p
    })
    return v
  })
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 累计出库
function showTotal(row) {
  drawerVisible.value = true
  info.value = row
}

// 当月出库
function showMonth(row) {
  monthDrawerVisible.value = true
  monthInfo.value = row
}

// 任务量
function showTask(row) {
  taskDrawerVisible.value = true
  taskInfo.value = row
}
// 工序在制品统计
function getProcessDetail(row, item) {
  processDrawerVisible.value = true
  processInfo.value = item
  processData.value = row
}
// 制成品详情
function getUps(row) {
  upsDrawerVisible.value = true
  upsInfo.value = row
}
</script>
<style lang="scss" scoped>
</style>

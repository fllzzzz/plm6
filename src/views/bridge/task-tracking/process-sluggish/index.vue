<template>
  <div class="app-container">
    <div class="app-wrap">
      <div class="project-chart">
        <div class="head-container">
          <mHeader />
        </div>
        <production-line-detail :workShopId="crud.query.workShopId" @change="processDetailChange" />
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div>
      <!--表格渲染-->
      <div class="content" style="padding: 0 0 0 20px">
        <div v-show="!processList.process?.id">
          <div class="my-code">*点击左侧未完成任务的工序图表查看详情</div>
        </div>
        <div v-show="processList.process?.id">
          <div style="display: flex; justify-content: space-between">
            <div class="head-container">
              <el-tag class="filter-item">工序：{{ processList.process?.name }}</el-tag>
              <project-cascader v-model="projectId" clearable class="filter-item" style="width: 300px" />
              <common-radio-button
                v-model="groupId"
                :options="groupData"
                type="other"
                class="filter-item"
                showOptionAll
                :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
                size="small"
                @change="crud.toQuery"
              />
              <div>
                <monomer-select-area-select
                  v-model:monomerId="monomerId"
                  v-model:areaId="areaId"
                  needConvert
                  clearable
                  :project-id="projectId"
                  style="width: 100px !important"
                />
                <el-input
                  v-model.trim="serialNumber"
                  size="small"
                  placeholder="输入编号搜索"
                  style="width: 170px"
                  class="filter-item"
                  clearable
                />
                <!-- <el-input
                  v-model.trim="groupName"
                  placeholder="输入班组搜索"
                  class="filter-item"
                  style="width: 170px"
                  size="small"
                  clearable
                  @keyup.enter="handleChange"
                /> -->
                <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">
                  搜索
                </common-button>
                <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
                  重置
                </common-button>
              </div>
            </div>
            <print-table
              v-permission="permission.print"
              api-key="mesProcessList"
              :params="{
                productionLineId: crud.query.productionLineId,
                productType: crud.query.productType,
                processId: crud.query.processId,
                projectId: crud.query.projectId,
                monomerId: crud.query.monomerId,
                areaId: crud.query.areaId,
                serialNumber: crud.query.serialNumber,
                groupId: crud.query.groupId,
              }"
              style="width: 300px"
              size="mini"
              type="warning"
            />
          </div>
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            :data-format="dataFormat"
            return-source-data
            :empty-text="crud.emptyText"
            :max-height="maxHeight - 50"
            style="width: 100%"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
            <el-table-column
              v-if="columns.visible('project')"
              key="project.shortName"
              prop="project"
              :show-overflow-tooltip="true"
              min-width="160"
              label="项目"
            >
              <template v-slot="scope">
                <span>{{ projectNameFormatter(scope.row.project) }}</span>
              </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('monomer.name')"
              key="monomer.name"
              prop="monomer.name"
              :show-overflow-tooltip="true"
              min-width="120"
              align="center"
              label="单体"
            />
            <el-table-column
              v-if="columns.visible('area.name')"
              :show-overflow-tooltip="true"
              key="area.name"
              prop="area.name"
              label="区域"
              align="center"
              min-width="100"
            />
            <el-table-column
              v-if="columns.visible('serialNumber')"
              :show-overflow-tooltip="true"
              key="serialNumber"
              prop="serialNumber"
              label="编号"
              align="center"
              min-width="100"
            />
            <el-table-column
              v-if="columns.visible('specification')"
              :show-overflow-tooltip="true"
              key="specification"
              prop="specification"
              label="规格"
              align="center"
              min-width="100"
            />
            <el-table-column
              v-if="columns.visible('unQuantity')"
              :show-overflow-tooltip="true"
              key="unQuantity"
              prop="unQuantity"
              label="未完成数"
              align="center"
            />
            <el-table-column
              v-if="columns.visible('weight')"
              :show-overflow-tooltip="true"
              key="weight"
              prop="weight"
              label="单重"
              align="center"
            />
            <el-table-column
              v-if="columns.visible('completeDate')"
              :show-overflow-tooltip="true"
              key="completeDate"
              prop="completeDate"
              label="完成日期"
              align="center"
            >
              <template v-slot="scope">
                <span>{{ scope.row.completeDate ? parseTime(scope.row.completeDate, '{y}-{m}-{d}') : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column
              v-if="columns.visible('groupName')"
              :show-overflow-tooltip="true"
              key="groupName"
              prop="groupName"
              label="负责班组"
              align="center"
            />
          </common-table>
          <!--分页组件-->
          <pagination />
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { get } from '@/api/bridge/bridge-task-tracking/process-sluggish.js'
import { ref, watch, provide, computed } from 'vue'
import { bridgeProcessSluggishPM as permission } from '@/page-permission/bridge'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import pagination from '@crud/Pagination'
import { processCategoryEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import projectCascader from '@comp-base/project-cascader.vue'
import mHeader from './module/header'
import productionLineDetail from './production-line-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['startDate', ['parse-time', '{y}-{m}-{d}']],
  ['endDate', ['parse-time', '{y}-{m}-{d}']],
  ['completeDate', ['parse-time', '{y}-{m}-{d}']]
])

const tableRef = ref()
const groupId = ref()
const groupData = ref([])
const processList = ref({})
const projectId = ref()
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

const workShopId = computed(() => {
  return crud.query.workShopId
})

provide('workShopId', workShopId)

const { crud, CRUD, columns } = useCRUD(
  {
    title: '工序呆滞',
    sort: [],
    optShow: { ...optShow },
    crudApi: { get },
    permission: { ...permission },
    invisibleColumns: [],
    requiredQuery: ['processId', 'productType'],
    hasPagination: true
  },
  tableRef
)

watch(
  () => projectId.value,
  (val) => {
    crud.query.projectId = projectId.value
    crud.toQuery()
  }
)
watch(
  () => groupId.value,
  (val) => {
    crud.query.groupId = val
    crud.toQuery()
  }
)
watch(
  () => monomerId.value,
  (val) => {
    crud.query.monomerId = monomerId.value
    crud.toQuery()
  }
)
watch(
  () => areaId.value,
  (val) => {
    crud.query.areaId = areaId.value
    crud.toQuery()
  }
)
watch(
  () => serialNumber.value,
  (val) => {
    crud.query.serialNumber = serialNumber.value
    crud.toQuery()
  }
)

watch(
  () => workShopId.value,
  (val) => {
    processList.value = {}
    crud.toQuery()
  }
)

const { maxHeight } = useMaxHeight({
  paginate: true
})

function processDetailChange(val) {
  processList.value = val
  crud.query.processId = val?.process?.id
  crud.query.productType = val?.productType
  groupId.value = undefined
  if (val?.process?.type === processCategoryEnum.DRILL_HOLE.V) {
    crud.query.productionLineId = undefined
  } else {
    crud.query.productionLineId = val?.productionLine?.id
  }

  groupData.value = val?.groupList
  crud.toQuery()
}

// 搜索
function searchQuery() {
  crud.toQuery()
}
// 重置
function resetQuery() {
  monomerId.value = undefined
  areaId.value = undefined
  projectId.value = undefined
  serialNumber.value = undefined
  groupId.value = undefined
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, res) => {}
</script>

<style lang="scss" scoped>
.app-wrap {
  display: flex;
  .project-chart {
    width: 25%;
  }
  .content {
    flex: 1;
    min-width: 0;
  }
}
</style>

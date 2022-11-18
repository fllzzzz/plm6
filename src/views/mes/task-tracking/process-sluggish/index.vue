<template>
  <div class="app-container">
    <div class="app-wrap">
      <div class="project-chart">
        <div class="head-container">
          <mHeader />
        </div>
        <projectChart v-model:year="year" @change="processChange" @success="handleEchartsData" />
      </div>
      <!--表格渲染-->
      <div class="content" style="padding: 0 0 0 20px">
        <div v-show="!processList.name">
          <div class="my-code">*点击左侧未完成任务的工序图表查看详情</div>
        </div>
        <div v-show="processList.name">
          <div style="display: flex; justify-content: space-between">
            <div class="head-container">
              <el-tag size="large" class="filter-item">工序：{{ processList.name }}</el-tag>
              <project-cascader v-model="projectId" clearable class="filter-item" style="width: 300px" />
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
                  @keyup.enter="handleChange"
                />
                <el-input
                  v-model.trim="groupName"
                  placeholder="输入班组搜索"
                  class="filter-item"
                  style="width: 170px"
                  size="small"
                  clearable
                  @keyup.enter="handleChange"
                />
                <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">
                  搜索
                </common-button>
                <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
                  重置
                </common-button>
              </div>
            </div>
            <print-table
              api-key="mesProcessList"
              :params="{
                productionLineId: crud.query.productionLineId,
                productType: crud.query.productType,
                processId: crud.query.processId,
                projectId: crud.query.projectId,
                monomerId: crud.query.monomerId,
                areaId: crud.query.areaId,
                serialNumber: crud.query.serialNumber,
                groupName: crud.query.groupName,
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
              min-width="150"
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
              min-width="140"
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
import { processSluggish, get } from '@/api/mes/task-tracking/process-sluggish.js'
import { ref, watch, provide, reactive, computed } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import pagination from '@crud/Pagination'
import { projectNameFormatter } from '@/utils/project'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import projectCascader from '@comp-base/project-cascader.vue'
import mHeader from './module/header'
import projectChart from './project-chart'

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

// 项目汇总数据（子页面使用）
const projectInfo = reactive({
  summary: {}, // 项目汇总数量
  provinceList: [], // 项目数量汇总
  loading: true
})

const tableRef = ref()
const year = ref() // 年份（子组件使用）
const processList = ref({})
const productionLineId = ref()
const projectId = ref()
const monomerId = ref()
const areaId = ref()
const groupName = ref()
const serialNumber = ref()

const searchProductType = computed(() => {
  return crud.query.searchProductType
})
const workShopId = computed(() => {
  return crud.query.workShopId
})

provide('projectInfo', projectInfo)
provide('searchProductType', searchProductType)
provide('workShopId', workShopId)

const { crud, CRUD, columns } = useCRUD(
  {
    title: '工序呆滞',
    sort: [],
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: [],
    requiredQuery: ['processId', 'productType'],
    hasPagination: true
  },
  tableRef
)

watch(
  () => productionLineId.value,
  (val) => {
    fetchProjectInfo()
    crud.toQuery()
  },
  { deep: true }
)

watch(
  () => projectId.value,
  (val) => {
    crud.query.projectId = projectId.value
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
  () => groupName.value,
  (val) => {
    crud.query.groupName = groupName.value
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

const { maxHeight } = useMaxHeight({
  paginate: true
})

// 获取工序呆滞列表数据
async function fetchProjectInfo() {
  projectInfo.provinceList = []
  if (!productionLineId.value) {
    return
  }
  projectInfo.loading = true
  try {
    const data =
      (await processSluggish({
        productType: searchProductType.value,
        workShopId: workShopId.value,
        productionLineId: productionLineId.value
      })) || {}
    projectInfo.provinceList = data
    delete data.provinceList
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}

function handleEchartsData(val) {
  processList.value = val
  console.log(val?.data)
  crud.query.processId = val?.data?.id
  crud.query.productType = val?.data?.productType
  crud.query.productionLineId = productionLineId.value
  crud.toQuery()
}

function processChange(val) {
  productionLineId.value = val
  fetchProjectInfo()
}

function handleChange() {
  crud.query.monomerId = monomerId.value
  crud.query.areaId = areaId.value
  crud.query.groupName = groupName.value
  crud.query.serialNumber = serialNumber.value
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
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, res) => {}
</script>

<style lang="scss" scoped>
.app-wrap {
  display: flex;
  .project-chart {
    border-right: 1px solid #ededed;
  }
  .content {
    flex: 1;
    min-width: 0;
  }
}
</style>

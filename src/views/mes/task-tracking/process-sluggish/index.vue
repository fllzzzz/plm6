<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div class="app-wrap">
      <div class="project-chart">
        <projectChart v-model:year="year" @change="processChange" @success="handleEchartsData" />
      </div>
      <!--表格渲染-->
      <div class="content" style="padding: 0 0 0 20px">
        <div v-show="!processList.name">
          <div class="my-code">*点击左侧未完成任务的工序图表查看详情</div>
        </div>
        <div v-show="processList.name">
          <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
            <el-tag style="align-self: center; font-weight: 900">工序：{{ processList.name }}</el-tag>
            <print-table api-key="mesProcessList" :params="{ productType: productType, processId: crud.query.processId }" size="mini" type="warning" />
          </div>
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            :data-format="dataFormat"
            return-source-data
            :empty-text="crud.emptyText"
            :max-height="maxHeight"
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
              v-if="columns.visible('monomerName')"
              key="monomerName"
              prop="monomerName"
              :show-overflow-tooltip="true"
              min-width="140"
              align="center"
              label="单体"
            />
            <el-table-column
              v-if="columns.visible('areaName')"
              key="areaName"
              prop="areaName"
              label="区域"
              align="center"
              min-width="100"
            />
            <el-table-column
              v-if="columns.visible('serialNumber')"
              key="serialNumber"
              prop="serialNumber"
              label="编号"
              align="center"
              min-width="100"
            />
            <el-table-column
              v-if="columns.visible('specification')"
              key="specification"
              prop="specification"
              label="规格"
              align="center"
              min-width="100"
            />
            <el-table-column v-if="columns.visible('unQuantity')" key="unQuantity" prop="unQuantity" label="未完成数" align="center" />
            <el-table-column v-if="columns.visible('weight')" key="weight" prop="weight" label="单重" align="center" />
            <el-table-column v-if="columns.visible('completeDate')" key="completeDate" prop="completeDate" label="完成日期" align="center">
              <template v-slot="scope">
                <span>{{ scope.row.completeDate ? parseTime(scope.row.completeDate, '{y}-{m}-{d}') : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column v-if="columns.visible('groupName')" key="groupName" prop="groupName" label="负责班组" align="center" />
          </common-table>
        </div>
        <!--分页组件-->
        <!-- <pagination /> -->
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
// import pagination from '@crud/Pagination'
import { projectNameFormatter } from '@/utils/project'
import mHeader from './module/header'
import projectChart from './project-chart'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false,
}
const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['startDate', ['parse-time', '{y}-{m}-{d}']],
  ['endDate', ['parse-time', '{y}-{m}-{d}']],
  ['completeDate', ['parse-time', '{y}-{m}-{d}']],
])

// 项目汇总数据（子页面使用）
const projectInfo = reactive({
  summary: {}, // 项目汇总数量
  provinceList: [], // 项目数量汇总
  loading: true,
})

const tableRef = ref()
const year = ref() // 年份（子组件使用）
const processList = ref({})
const productionLineId = ref()

const productType = computed(() => {
  return crud.query.productType
})
const workShopId = computed(() => {
  return crud.query.workShopId
})

provide('projectInfo', projectInfo)
provide('productType', productType)
provide('workShopId', workShopId)

const { crud, CRUD, columns } = useCRUD(
  {
    title: '工序呆滞',
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: [],
    hasPagination: false,
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

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.content'],
  wrapperBox: ['.app-wrap'],
  paginate: true,
})

// 获取工序呆滞列表数据
async function fetchProjectInfo() {
  projectInfo.loading = true
  try {
    const data =
      (await processSluggish({ productType: productType.value, workShopId: workShopId.value, productionLineId: productionLineId.value })) ||
      {}
    projectInfo.provinceList = data
    delete data.provinceList
    // projectInfo.summary = data
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}

function handleEchartsData(val) {
  processList.value = val
  console.log(val?.data?.id,'val');
  crud.query.processId = val?.data?.id
  crud.toQuery()
}

function processChange(val) {
  productionLineId.value = val
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data
}
</script>

<style lang="scss" scoped>
.app-wrap {
  display: flex;
  .content {
    flex: 1;
    min-width: 0;
  }
}
</style>

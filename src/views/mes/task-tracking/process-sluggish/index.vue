<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div class="app-wrap">
      <div class="project-chart">
        <projectChart v-model:year="year" @success="handleEchartsData" />
      </div>
      <!--工具栏-->
      <!--表格渲染-->
      <div class="content" style="padding: 0 0 0 20px">
        <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
          <el-tag style="align-self: center; font-weight: 900">工序：{{ processList.data }}</el-tag>
          <print-table api-key="processList" :params="{ year: crud.query.year }" size="mini" type="warning" />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :data-format="dataFormat"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          style="width: 100%"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
          <el-table-column
            v-if="columns.visible('name')"
            key="name"
            prop="name"
            :show-overflow-tooltip="true"
            min-width="150"
            align="center"
            label="项目名称"
          >
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}-{{ scope.row.name }}</span>
          </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('monomer')"
            key="monomer"
            prop="monomer"
            :show-overflow-tooltip="true"
            min-width="140"
            align="center"
            label="单体"
          />
          <el-table-column v-if="columns.visible('area')" key="area" prop="area" label="区域" align="center" min-width="100" />
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
          <el-table-column
            v-if="columns.visible('unFinishQuantity')"
            key="unFinishQuantity"
            prop="unFinishQuantity"
            label="未完成数"
            align="center"
            min-width="100"
          />
          <el-table-column v-if="columns.visible('weight')" key="weight" prop="weight" label="单重" align="center" min-width="100" />
          <el-table-column v-if="columns.visible('endDate')" key="endDate" prop="endDate" label="完成日期" align="center" width="110" />
          <el-table-column v-if="columns.visible('team')" key="team" prop="team" label="负责班组" align="center" width="130" />
        </common-table>
        <!--分页组件-->
        <pagination />
      </div>
    </div>
  </div>
</template>

<script setup>
import { getProjectList as get, getProjectInfo } from '@/api/common'
import { ref, watch, provide, reactive } from 'vue'

// import { dateDifference, dateDifferenceReduce } from '@/utils/date'
// import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import { myProjectPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
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

provide('projectInfo', projectInfo)

const { crud, columns } = useCRUD(
  {
    title: '工序呆滞',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: [],
    hasPagination: true
  },
  tableRef
)

watch(
  () => year,
  (val) => {
    fetchProjectInfo()
    if (crud.query.year !== val.value) {
      crud.query.year = val.value
      crud.toQuery()
    }
  },
  { deep: true }
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.content'],
  wrapperBox: ['.app-wrap'],
  paginate: true
})

// 获取项目汇总数据
async function fetchProjectInfo() {
  if (!checkPermission(permission.statistics)) return
  projectInfo.loading = true
  try {
    const res = (await getProjectInfo({ year: year.value })) || {}
    projectInfo.provinceList = res.provinceList
    delete res.provinceList
    projectInfo.summary = res
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}

function handleEchartsData(val) {
  processList.value = val
  console.log(processList.value, 'processList.value')
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

<template>
  <div class="app-wrap">
    <div class="project-chart">
      <projectChart v-model:year="year" />
    </div>
    <div class="app-container">
      <!--工具栏-->
      <div class="head-container">
        <mHeader />
      </div>
      <!--表格渲染-->
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
          v-if="columns.visible('serialNumber')"
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          min-width="130"
          align="center"
          label="合同编号"
          fixed="left"
        />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          min-width="150"
          align="center"
          label="项目名称"
        />
        <el-table-column
          v-if="columns.visible('shortName')"
          key="shortName"
          prop="shortName"
          :show-overflow-tooltip="true"
          min-width="140"
          align="center"
          label="项目简称"
        />
        <el-table-column
          v-if="columns.visible('businessType')"
          key="businessType"
          prop="businessType"
          label="业务类型"
          align="center"
          min-width="100"
        >
          <template v-slot="scope">
            <span>{{ scope.row.businessType && businessTypeEnum.VL[scope.row.businessType] }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" width="90" align="center">
          <template v-slot="scope">
            <span>{{ projectStatusEnum.VL[scope.row.status] }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('startDate')"
          key="startDate"
          prop="startDate"
          label="计划开工日期"
          align="center"
          width="110"
        />
        <el-table-column v-if="columns.visible('endDate')" key="endDate" prop="endDate" label="计划完成日期" align="center" width="110" />
        <el-table-column
          v-if="columns.visible('completeDate')"
          key="completeDate"
          prop="completeDate"
          label="完工日期"
          align="center"
          width="110"
        />
        <el-table-column v-if="columns.visible('allDays')" key="allDays" prop="allDays" label="工期(天)" align="center" width="100" />
        <el-table-column
          v-if="columns.visible('alreadyDays')"
          key="alreadyDays"
          prop="alreadyDays"
          label="已用时(天)"
          align="center"
          width="100"
        />
        <el-table-column
          v-if="columns.visible('createTime')"
          key="createTime"
          prop="createTime"
          label="创建时间"
          align="center"
          width="130"
        />
      </common-table>
      <!--分页组件-->
      <pagination />
    </div>
  </div>
</template>

<script setup>
import { getProjectList as get, getProjectInfo } from '@/api/common'
import { ref, watch, provide, reactive } from 'vue'

import { dateDifference } from '@/utils/date'
import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import { bridgeMyProjectPM as permission } from '@/page-permission/bridge'
import { businessTypeEnum, projectStatusEnum, projectTypeEnum } from '@enum-ms/contract'

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
  provinceList: [], // 省份项目数量汇总
  loading: true
})

const tableRef = ref()
const year = ref() // 年份（子组件使用）

provide('projectInfo', projectInfo)

const { crud, columns, CRUD } = useCRUD(
  {
    title: '我的项目',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: ['name', 'completeDate', 'createTime'],
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

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    // 计算天数
    v.allDays = ''
    v.alreadyDays = ''
    if (isNotBlank(v.startDate)) {
      // 工期
      if (isNotBlank(v.endDate)) {
        v.allDays = dateDifference(v.startDate, v.endDate)
      }
      // 用时天数（清单内所有任务全部入库，自动停止计时）
      const completeDate = v.completeDate || new Date().getTime()
      v.alreadyDays = v.completeDate === v.endDate ? dateDifference(v.startDate, v.endDate) : dateDifference(v.startDate, completeDate)
    }
    return v
  })
}

// 获取项目汇总数据
async function fetchProjectInfo() {
  if (!checkPermission(permission.statistics)) return
  projectInfo.loading = true
  try {
    const res = (await getProjectInfo({ year: year.value, projectType: projectTypeEnum.BRIDGE.V })) || {}
    projectInfo.provinceList = res.provinceList
    delete res.provinceList
    projectInfo.summary = res
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}
</script>

<style lang="scss" scoped>
.app-wrap {
  display: flex;
  .app-container {
    flex: 1;
    min-width: 0;
  }
}
</style>

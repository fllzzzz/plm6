<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />

      <el-table-column
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="100"
        header-align="center"
      >
        <template v-slot="scope">
          <span>{{scope.row.projectNumber}}-{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomerNum')"
        key="monomerNum"
        prop="monomerNum"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="60"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomerNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('areaNum')"
        key="areaNum"
        prop="areaNum"
        :show-overflow-tooltip="true"
        label="单元"
        min-width="60"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ scope.row.areaNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('sum')"
        key="sum"
        prop="sum"
        align="center"
        :show-overflow-tooltip="true"
        label="零件数量（件）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reduce')"
        key="reduce"
        prop="reduce"
        align="center"
        :show-overflow-tooltip="true"
        label="零件重量（kg）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reduce }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('nestingState')"
        key="nestingState"
        prop="nestingState"
        :show-overflow-tooltip="true"
        label="状态"
        align="center"
        min-width="60"
      >
        <template v-slot="scope">
          <el-tag style="width: 100%" v-if="scope.row.nestingState === 0" type="danger">未排套</el-tag>
          <el-tag style="width: 100%" v-if="scope.row.nestingState && scope.row.nestingState === 1" type="warning">部分排套</el-tag>
          <el-tag style="width: 100%" v-if="scope.row.nestingState && scope.row.nestingState === 2" type="success">排套结束</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('importTime')"
        key="importTime"
        align="center"
        prop="importTime"
        :show-overflow-tooltip="true"
        label="创建时间"
        min-width="60"
      >
        <template v-slot="scope">
          <span> {{ scope.row.importTime }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.updateTime }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.createTime }}</span>
        </template>
      </el-table-column> -->
      
      <el-table-column v-if="checkPermission(permission.detail)" :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-view" @click="viewDetails(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <detail :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import { watch,reactive,provide } from 'vue'
import {ref} from 'vue'
import crudApi from '@/api/cutting/radan-controller'
import { getProjectInfo } from '@/api/cutting/radan-controller'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import { parseTime } from '@/utils/date'
import pagination from '@crud/Pagination'
import detail from './detail/index.vue'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { nestingTaskPM as permission } from '@/page-permission/cutting'


const specsVisible = ref(false)
const tableRef = ref()
const detailObj = ref([])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 项目汇总数据（子页面使用）
const projectInfo = reactive({
  summary: {}, // 项目汇总数量
  provinceList: [], // 省份项目数量汇总
  loading: true
})

provide('projectInfo', projectInfo)

const { crud, columns} = useCRUD(
  {
    title: '套料任务',
    sort: ['createTime.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

watch(
  // () => crud.query.year,
  (val) => {
    fetchProjectInfo()
    crud.toQuery()
  },
  { deep: true }
)

const { maxHeight } = useMaxHeight({
  // wrapperBox: '.contractRecord',
  paginate: true,
  // extraHeight: 40
})
// 获取项目汇总数据
async function fetchProjectInfo() {
  if (!checkPermission(permission.statistics)) return
  projectInfo.loading = true
  try {
    const res = (await getProjectInfo({ year: crud.query.year })) || {}
    projectInfo.provinceList = res.provinceList
    delete res.provinceList
    projectInfo.summary = res
  } catch (error) {
    console.log('获取项目汇总图表数据', error)
  } finally {
    projectInfo.loading = false
  }
}
// 查看详情
function viewDetails(row) {
  specsVisible.value = true
  detailObj.value = row
  console.log(row)
}

</script>

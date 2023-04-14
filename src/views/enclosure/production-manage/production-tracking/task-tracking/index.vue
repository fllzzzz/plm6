<template>
  <div class="wrap">
    <div class="wrap-left">
      <project-list @project-change="projectChange" />
    </div>
    <div class="wrap-right">
      <div class="app-container">
        <el-tag v-if="!crud.query?.projectId" type="info" effect="plain" size="large"> * 请点击左侧项目列表查看详情 </el-tag>
        <template v-else>
          <!--工具栏-->
          <mHeader class="head-container task-head-container">
            <template #viewLeft>
              <el-tag effect="plain" size="medium">
                <span v-parse-project="{ project: project }" v-empty-text />
              </el-tag>
            </template>
          </mHeader>
          <!--表格渲染-->
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            :empty-text="crud.emptyText"
            :max-height="maxHeight"
            :data-format="dataFormat"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column
              key="createTime"
              prop="createTime"
              v-if="columns.visible('createTime')"
              show-overflow-tooltip
              label="排产日期"
              align="center"
              width="90"
            />
            <el-table-column
              key="orderNumber"
              prop="orderNumber"
              v-if="columns.visible('orderNumber')"
              label="任务工单"
              show-overflow-tooltip
              align="center"
              min-width="130"
            />
            <el-table-column
              key="userName"
              prop="userName"
              v-if="columns.visible('userName')"
              show-overflow-tooltip
              label="排产人"
              align="center"
              width="90"
            />
            <el-table-column
              key="category"
              prop="category"
              v-if="columns.visible('category')"
              show-overflow-tooltip
              label="类型"
              align="center"
              width="110"
            >
              <template v-slot="{ row }">
                <el-tag effect="plain" size="medium">{{ row.category }}</el-tag>
              </template>
            </el-table-column>
            <el-table-column
              key="factoryName"
              prop="factoryName"
              v-if="columns.visible('factoryName')"
              show-overflow-tooltip
              label="工厂"
              align="center"
              min-width="100"
            />
            <el-table-column
              key="workshopName"
              prop="workshopName"
              v-if="columns.visible('workshopName')"
              show-overflow-tooltip
              label="车间"
              align="center"
              min-width="100"
            />
            <el-table-column
              key="productionLineName"
              prop="productionLineName"
              v-if="columns.visible('productionLineName')"
              show-overflow-tooltip
              label="生产线"
              align="center"
              min-width="130"
            >
              <template v-slot="{ row }"> {{ row.productionLineName }} / {{ row.leaderName }} </template>
            </el-table-column>
            <el-table-column
              key="quantity"
              prop="quantity"
              v-if="columns.visible('quantity')"
              show-overflow-tooltip
              label="任务数(件/m)"
              align="center"
              min-width="100"
            >
              <template v-slot="{ row }">
                <span>{{ row.quantity }} / {{ row.totalLength }}</span>
              </template>
            </el-table-column>
            <el-table-column
              key="completeQuantity"
              prop="completeQuantity"
              v-if="columns.visible('completeQuantity')"
              show-overflow-tooltip
              label="完成数(件/m)"
              align="center"
              min-width="100"
            >
              <template v-slot="{ row }">
                <span>{{ row.completeQuantity }} / {{ row.completeLength }}</span>
              </template>
            </el-table-column>
            <el-table-column
              key="booleanlag"
              prop="booleanlag"
              v-if="columns.visible('booleanlag')"
              show-overflow-tooltip
              label="状态"
              align="center"
              width="70"
            >
              <template v-slot="{ row }">
                <el-tag v-if="!!row.booleanlag" effect="plain" size="medium" type="success">正常</el-tag>
                <el-tag v-else effect="plain" size="medium" type="danger">滞后</el-tag>
              </template>
            </el-table-column>
            <el-table-column
              key="completeRate"
              prop="completeRate"
              v-if="columns.visible('completeRate')"
              show-overflow-tooltip
              label="进度"
              align="center"
              min-width="90"
            >
              <template v-slot="{ row }">
                <el-progress :text-inside="true" :stroke-width="29" :percentage="row.completeRate" />
              </template>
            </el-table-column>
            <!--详情-->
            <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="70px" align="center" fixed="right">
              <template #default="{ row }">
                <udOperation :data="row" :show-edit="false" :show-del="false" show-detail />
              </template>
            </el-table-column>
          </common-table>
          <!--分页组件-->
          <pagination />
          <!-- 查看详情 -->
          <m-detail :project="project" />
        </template>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-manage/task-tracking'
import { ref } from 'vue'

import { enclosureTaskTrackingPM as permission } from '@/page-permission/enclosure'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import UdOperation from '@crud/UD.operation'
import useCRUD from '@compos/use-crud'
import projectList from './project-list'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import MDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const project = ref({})
const dataFormat = ref([
  ['totalLength', ['to-fixed', 2]],
  ['completeLength', ['to-fixed', 2]],
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['category', ['parse-enum', mesEnclosureTypeEnum]]
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '任务跟踪',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId'],
    invisibleColumns: []
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: '.task-head-container',
  paginate: true
})

function projectChange(row = {}) {
  project.value = row
  crud.query.projectId = row.id
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.completeRate = Number(toFixed((row.completeLength / row.totalLength) * 100, 2))
    row.totalLength = (row.totalLength || 0) / 1000
    row.completeLength = (row.completeLength || 0) / 1000
  })
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;

  .wrap-left {
    width: 394px;
  }

  .wrap-right {
    flex: 1;
    min-width: 400px;
    .app-container {
      padding-left: 0;
    }
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>

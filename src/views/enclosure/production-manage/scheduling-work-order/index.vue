<template>
  <div class="wrap">
    <div class="wrap-left">
      <project-list @project-change="projectChange" />
    </div>
    <div class="wrap-right">
      <div class="app-container">
        <el-tag v-if="!project.id" type="info" effect="plain" size="large"> * 请点击左侧项目列表查看详情 </el-tag>
        <template v-else>
          <!--工具栏-->
          <mHeader class="head-container scheduling-head-container">
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
            row-key="id"
            :data-format="dataFormat"
            @selection-change="crud.selectionChangeHandler"
          >
            <el-table-column type="selection" :selectable="selectable" width="55" align="center" />
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column
              key="createTime"
              prop="createTime"
              v-if="columns.visible('createTime')"
              show-overflow-tooltip
              label="排产日期"
              align="center"
              width="100"
            />
            <el-table-column
              key="orderNumber"
              prop="orderNumber"
              v-if="columns.visible('orderNumber')"
              label="任务工单"
              show-overflow-tooltip
              align="center"
              min-width="140"
            >
              <template v-slot="{ row }">
                <table-cell-tag :show="Boolean(row.printQuantity)" name="已打印" color="#e64242" :offset="15" />
                <span>{{ row.orderNumber }}</span>
              </template>
            </el-table-column>
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
              label="任务数(件)"
              align="center"
              width="80"
            />
            <el-table-column
              key="totalLength"
              prop="totalLength"
              v-if="columns.visible('totalLength')"
              show-overflow-tooltip
              label="任务量(m)"
              align="right"
              width="80"
            />
            <!--编辑与删除-->
            <el-table-column
              v-if="checkPermission([...permission.detail, ...permission.del])"
              label="操作"
              width="120px"
              align="center"
              fixed="right"
            >
              <template #default="{ row }">
                <udOperation
                  :data="row"
                  :show-edit="false"
                  :show-del="!row.booleanProduce"
                  :show-detail="checkPermission(permission.detail)"
                />
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
import crudApi from '@/api/enclosure/production-manage/scheduling-work-order'
import { nextTick, ref } from 'vue'

import { enclosureSchedulingWorkOrderPM as permission } from '@/page-permission/enclosure'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import UdOperation from '@crud/UD.operation'
import useCRUD from '@compos/use-crud'
import projectList from './project-list'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import MDetail from './module/detail'
import tableCellTag from '@comp-common/table-cell-tag/index'

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const project = ref({})
const dataFormat = ref([
  ['totalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]],
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['category', ['parse-enum', mesEnclosureTypeEnum]]
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '排产工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId'],
    invisibleColumns: [],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: '.scheduling-head-container',
  paginate: true
})

function projectChange(row = {}) {
  project.value = row
  nextTick(() => {
    crud.query.projectId = row.id
    crud.toQuery()
  })
}

// 是否可以删除（生产后不能删除）
function selectable(row) {
  return !row.booleanProduce
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.totalLength = (row.totalLength || 0) / 1000
  })
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;

  .wrap-left {
    width: 454px;
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

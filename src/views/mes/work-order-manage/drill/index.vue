<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <drill-project-list :maxHeight="maxHeight - 40" @nesting-task-click="handleNestingTaskClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.areaIds?.length" type="info" size="medium"> * 请点击左侧项目列表查看详情 </el-tag>
      <div v-else>
        <div class="wrap-head">
          <mHeader />
        </div>
        <!-- 表格 -->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight - 130"
          style="width: 100%"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column
            v-if="columns.visible('scheduleTime')"
            align="center"
            key="scheduleTime"
            prop="scheduleTime"
            :show-overflow-tooltip="true"
            label="排产日期"
          >
            <template v-slot="scope">
              <table-cell-tag :show="scope.row.boolOffLine" name="线下套料" color="#E6A23C" />
              <span>{{ scope.row.scheduleTime ? parseTime(scope.row.scheduleTime, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('cutNumber')"
            header-align="center"
            key="cutNumber"
            prop="cutNumber"
            :show-overflow-tooltip="true"
            label="切割指令号"
            width="220px"
          >
            <template v-slot="scope">
              <table-cell-tag :show="scope.row.boolPrinted" color="#e64242" name="已打印" :offset="15" />
              <span>{{ scope.row.cutNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('userName')"
            align="center"
            key="userName"
            prop="userName"
            :show-overflow-tooltip="true"
            label="排产人"
          >
            <template v-slot="scope">
              <span>{{ scope.row.userName }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('config')"
            align="center"
            key="config.name"
            prop="config.name"
            :show-overflow-tooltip="true"
            label="下料方式"
          >
            <template v-slot="scope">
              <span>{{ scope.row.config?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('workshop')"
            align="center"
            key="workshop.name"
            prop="workshop.name"
            :show-overflow-tooltip="true"
            label="车间"
          >
            <template v-slot="scope">
              <span>{{ scope.row.workshop?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('productionLine')"
            align="center"
            key="productionLine.name"
            prop="productionLine.name"
            :show-overflow-tooltip="true"
            label="生产线"
          >
            <template v-slot="scope">
              <span>{{ scope.row.productionLine?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('taskQuantity')"
            align="center"
            key="taskQuantity"
            prop="taskQuantity"
            :show-overflow-tooltip="true"
            label="任务数（件）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.taskQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('taskNetWeight')"
            align="center"
            key="taskNetWeight"
            prop="taskNetWeight"
            :show-overflow-tooltip="true"
            label="总净重（kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.taskNetWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('taskGrossWeight')"
            align="center"
            key="taskGrossWeight"
            prop="taskGrossWeight"
            :show-overflow-tooltip="true"
            label="总毛重（kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.taskGrossWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('completeTime')"
            align="center"
            key="completeTime"
            prop="completeTime"
            :show-overflow-tooltip="true"
            label="完成日期"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeTime ? parseTime(scope.row.completeTime, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column v-permission="[...permission.detail]" align="center" :show-overflow-tooltip="true" label="操作" width="100">
            <template v-slot="scope">
              <common-button
                v-if="crud.query.processType === mesMachinePartOrderTypeEnum.DRILL_ORDER.V"
                type="primary"
                size="mini"
                @click="showDrill(scope.row)"
                >查看</common-button
              >
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <pagination />
      </div>
      <!-- 钻孔工单详情 -->
      <detail v-model:visible="drawerVisible" :process-type="crud.query.processType" :detail-data="detailData" :serialNumber="crud.query.serialNumber" @refresh="crud.toQuery" />
    </div>
  </div>
</template>
<script setup>
import { ref, provide } from 'vue'
import crudApi from '@/api/mes/work-order-manage/machine-part.js'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import { parseTime } from '@/utils/date'
// import { debounce } from '@/utils'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { drillWorkOrderPM as permission } from '@/page-permission/mes'
import mHeader from '../components/header.vue'
import detail from './module/detail.vue'
import drillProjectList from './module/drill-project-list.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const detailData = ref({}) // 钻孔
const drawerVisible = ref(false)

const { crud, CRUD, columns } = useCRUD(
  {
    title: '钻孔工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['taskGrossWeight'],
    requiredQuery: ['processType', 'areaIds'],
    hasPagination: true
  },
  tableRef
)

provide('crud', crud)
provide('permission', permission)

const { maxHeight } = useMaxHeight()

// 查看钻孔工单详情
function showDrill(row) {
  drawerVisible.value = true
  detailData.value = row
}

// const handleNestingTaskClick = debounce(function (nodes = []) {
//   crud.query.processType = mesMachinePartOrderTypeEnum.DRILL_ORDER.V
//   if (nodes?.length) {
//     crud.query.areaId = nodes[0].id
//     crud.query.projectId = nodes[0].projectId
//   } else {
//     crud.query.areaId = undefined
//     crud.query.projectId = undefined
//   }
//   crud.toQuery()
// }, 500)

function handleNestingTaskClick({ projectIds, monomerIds, areaIds }) {
  crud.query.processType = mesMachinePartOrderTypeEnum.DRILL_ORDER.V
  crud.query.projectIds = projectIds
  crud.query.monomerIds = monomerIds
  crud.query.areaIds = areaIds
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.boolPrinted = Boolean(v.printQuantity)
    return v
  })
}
</script>
<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 400px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
</style>

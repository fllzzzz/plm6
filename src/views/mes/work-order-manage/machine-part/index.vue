<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <part-project-list :maxHeight="maxHeight - 40" @nesting-task-click="handleNestingTaskClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.processType" type="info" size="medium"> * 请点击左侧项目列表查看详情 </el-tag>
      <div v-else>
        <div class="head-container">
          <mHeader />
        </div>
        <!-- 表格 -->
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
            label="任务量（件/kg）"
          >
            <template v-slot="scope">
              <span>{{ scope.row.taskQuantity }}/{{ scope.row.taskNetWeight }}</span>
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
              <!-- <common-button
                v-if="crud.query.processType === mesMachinePartOrderTypeEnum.DRILL_ORDER.V"
                type="primary"
                size="mini"
                @click="showDrill(scope.row)"
                >查看</common-button
              > -->
              <common-button v-if="crud.query.processType === mesMachinePartOrderTypeEnum.CUTTING_ORDER.V" type="primary" size="mini" @click="showCuttingDetail(scope.row)">查看</common-button>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <pagination />
      </div>
      <!-- 钻孔工单详情 -->
      <!-- <detail v-model:visible="drawerVisible" :process-type="crud.query.processType" :detail-data="detailData" /> -->
      <!-- 切割工单详情 -->
      <cutting-detail
        :process-type="crud.query.processType"
        v-model:visible="cuttingDrawerVisible"
        :cutting-detail-data="cuttingDetailData"
        @refresh="crud.toQuery"
      />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/work-order-manage/machine-part.js'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import { parseTime } from '@/utils/date'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { machinePartWorkOrderPM as permission } from '@/page-permission/mes'
import mHeader from './module/header.vue'
// import detail from './module/detail.vue'
import cuttingDetail from './module/cutting-detail.vue'
import partProjectList from './module/part-project-list.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
// const detailData = ref({}) // 钻孔
// const drawerVisible = ref(false)
const cuttingDetailData = ref({})
const cuttingDrawerVisible = ref(false)

const { crud, CRUD, columns } = useCRUD(
  {
    title: '零件工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['processType'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  extraHeight: 15,
  paginate: true
})

// 预览切割工单 pdf
function showCuttingDetail(row) {
  console.log(row, 'row')
  cuttingDrawerVisible.value = true
  cuttingDetailData.value = row
}

// 查看钻孔工单详情
// function showDrill(row) {
//   drawerVisible.value = true
//   detailData.value = row
// }

function handleNestingTaskClick(val, query, year) {
  crud.query.projectId = val?.id
  crud.query.localDateTime = year
  crud.query.processType = mesMachinePartOrderTypeEnum.CUTTING_ORDER.V
  // if (crud.query.projectId) {
  //   crud.toQuery()
  // }
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
    width: 380px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
</style>

<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <nesting-task-list
        ref="nestingTaskRef"
        :heightStyle="heightStyle"
        :maxHeight="maxHeight - 40"
        @nesting-task-click="handleNestingTaskClick"
      />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.areaId" type="info" size="medium"> * 请先选择套料任务，进行零件任务下发 </el-tag>
      <div v-else>
        <div class="head-container">
          <mHeader />
        </div>
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight - 90"
          row-key="rowKey"
          style="width: 100%"
        >
          <el-table-column
            prop="createTime"
            v-if="columns.visible('createTime')"
            :show-overflow-tooltip="true"
            label="排产日期"
            align="center"
          >
            <template #default="{ row }">
              <span>{{ parseTime(row.createTime, '{y}-{m}-{d}') }}</span>
            </template>
          </el-table-column>
          <el-table-column
            prop="orderNumber"
            v-if="columns.visible('orderNumber')"
            :show-overflow-tooltip="true"
            label="任务单号"
            min-width="100"
            align="center"
          >
            <template #default="{ row }">
              <table-cell-tag :show="row.boolOffLine === !!nestingTypeEnum.OFFLINE.V" name="线下套料" color="#E6A23C" />
              <span @click="toDetail(row)" style="color: #409eff; cursor: pointer">{{ row.orderNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            prop="thick"
            v-if="columns.visible('thick')"
            :show-overflow-tooltip="true"
            label="厚度"
            align="center"
            width="80"
          />
          <el-table-column prop="material" v-if="columns.visible('material')" :show-overflow-tooltip="true" label="材质" align="center" />
          <el-table-column
            prop="totalNetWeight"
            v-if="columns.visible('totalNetWeight')"
            :show-overflow-tooltip="true"
            label="零件总量（件/kg）"
            align="center"
            width="140"
          >
            <template #default="{ row }">
              <span @click="toDetail(row)" style="color: #409eff; cursor: pointer">{{ row.quantity }} / {{ row.totalNetWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column
            prop="usedWeight"
            v-if="columns.visible('usedWeight')"
            :show-overflow-tooltip="true"
            label="钢板使用量"
            align="center"
            width="100"
          >
            <template #default="{ row }">
              <span>{{ row.usedWeight }}</span>
            </template>
          </el-table-column>
          <!-- <el-table-column
            v-if="columns.visible('boolNestCutEnum')"
            :show-overflow-tooltip="true"
            label="套料状态"
            align="center"
            width="100"
          >
            <template #default="{ row }">
              <template v-if="row.boolNestCutEnum">
                <el-tag v-if="row.issueStatusEnum" effect="plain" :type="issueStatusEnum.V[row.issueStatusEnum].T">{{
                  issueStatusEnum.VL[row.issueStatusEnum]
                }}</el-tag>
              </template>
              <template v-else>
                <el-tag effect="plain" type="danger">{{ layOffWayTypeEnum.VL[row.boolNestCutEnum] }}</el-tag>
              </template>
            </template>
          </el-table-column> -->
          <el-table-column v-if="columns.visible('issueStatusEnum')" :show-overflow-tooltip="true" label="状态" align="center" width="100">
            <template #default="{ row }">
              <el-tag v-if="row.issueStatusEnum" effect="plain" :type="issueStatusEnum.V[row.issueStatusEnum].T">{{
                issueStatusEnum.VL[row.issueStatusEnum]
              }}</el-tag>
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template #default="{ row }">
              <common-button v-permission="permission.document" class="filter-item" size="mini" type="primary" @click="toViews(row)">
                套料文档
              </common-button>
              <common-button
                v-permission="permission.scheduling"
                class="filter-item"
                size="mini"
                type="success"
                v-if="row.issueStatusEnum === issueStatusEnum.OUT_NESTING.V"
                @click="toBatchIssue(row)"
              >
                排产
              </common-button>
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <pagination />
      </div>
      <!-- <preview-dialog v-model:visible="previewVisible" :list="submitList" :info="info" @success="handleIssueSuccess" /> -->
      <part-dialog v-model:visible="partDialogVisible" :part-list="partList" @success="crud.toQuery" />
      <nesting-document-dialog v-model:visible="nestingDialogVisible" :nesting-list="nestingList" />
      <nesting-detail v-model:visible="detailDialogVisible" :detail-list="detailList" />
    </div>
  </div>
</template>

<script setup>
import { getNestingTask } from '@/api/mes/scheduling-manage/machine-part'
import { ref, provide } from 'vue'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/mes'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import { machinePartSchedulingIssueStatusEnum as issueStatusEnum, nestingTypeEnum } from '@enum-ms/mes'
import { debounce } from '@/utils'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import nestingTaskList from './module/nesting-task-list.vue'
// import previewDialog from './module/preview-dialog'
import partDialog from './module/part-dialog.vue'
import nestingDocumentDialog from './module/nesting-document-dialog.vue'
import nestingDetail from './module/nesting-detail.vue'

const partDialogVisible = ref(false)
const nestingDialogVisible = ref(false)
const nestingList = ref({})
const detailDialogVisible = ref(false)
const detailList = ref([])
const partList = ref([])

const areaId = ref()
const projectId = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '套料成果',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getNestingTask },
    invisibleColumns: [''],
    hasPagination: true,
    requiredQuery: ['areaId']
  },
  tableRef
)

provide('crud', crud)
provide('permission', permission)
const { maxHeight, heightStyle } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: false
})
const nestingTaskRef = ref()

CRUD.HOOK.beforeToQuery = async () => {
  crud.query.boolNestCutEnum = layOffWayTypeEnum.NESTING.V
}

const handleNestingTaskClick = debounce(function (nodes = []) {
  areaId.value = undefined
  projectId.value = undefined
  for (let x = 0; x < nodes.length; x++) {
    console.log(nodes[x].parentIds, 'x')
    areaId.value = nodes[x].id
    projectId.value = nodes[x].parentIds[1]
    console.log(projectId.value, 'projectId.value')
  }
  crud.query.areaId = areaId.value
  crud.query.projectId = projectId.value
  crud.toQuery()
}, 500)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

// 排产
function toBatchIssue(row) {
  partDialogVisible.value = true
  partList.value = row
}

// 套料文档弹窗
function toViews(row) {
  nestingDialogVisible.value = true
  nestingList.value = row
}

// 点击任务工单查看详情
function toDetail(row) {
  detailDialogVisible.value = true
  detailList.value = row
}

// function handleNestingTaskClick(val, query) {
//   crud.query.projectId = val?.id
//   if (crud.query.projectId) {
//     crud.toQuery()
//   }
// }
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 445px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
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

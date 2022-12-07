<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <nesting-task-list ref="nestingTaskRef" :maxHeight="maxHeight" @nesting-task-click="handleNestingTaskClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.id" type="info" size="medium"> * 请先选择套料任务，进行零件任务下发 </el-tag>
      <template v-else>
        <div class="head-container">
          <mHeader>
            <template #optRight>
              <common-button
              v-permission="permission.save"
                class="filter-item"
                :disabled="!crud.selections?.length"
                size="mini"
                icon="el-icon-edit"
                type="primary"
                @click="toBatchIssue"
              >
                任务下发
              </common-button>
            </template>
          </mHeader>
        </div>
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          :cell-class-name="wrongCellMask"
          row-key="rowKey"
          style="width: 100%"
          @selection-change="crud.selectionChangeHandler"
        >
          <el-table-column type="selection" width="55" align="center" class="selection" :selectable="selectable" />
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column
            v-if="columns.visible('cutNumber')"
            prop="cutNumber"
            :show-overflow-tooltip="true"
            label="切割指令号"
            min-width="160"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('spec') && currentNesting.boolNestCutEnum"
            prop="spec"
            :show-overflow-tooltip="true"
            label="原材料规格"
            min-width="120"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('num') && currentNesting.boolNestCutEnum"
            prop="num"
            :show-overflow-tooltip="true"
            label="板材数量(件)"
            width="100"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('orderNumber') && currentNesting.boolNestCutEnum"
            prop="orderNumber"
            :show-overflow-tooltip="true"
            label="套料文档"
            width="100"
            align="center"
          >
            <template #default="{ row }">
              <common-button size="mini" type="primary" @click="showPdf(row)">查看</common-button>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('cutName')"
            prop="cutName"
            :show-overflow-tooltip="true"
            label="切割方式"
            width="100"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('groupsId')"
            :show-overflow-tooltip="true"
            prop="groupsId"
            label="生产组"
            min-width="150px"
            align="center"
          >
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-cascader
                v-if="!row.boolIssueEnum"
                v-model="row.groupsId"
                :options="schedulingGroups.list"
                :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
                :show-all-levels="false"
                filterable
                clearable
                :placeholder="$index === 0 ? '请选择生产组' : '同上'"
                @change="handleGroupsChange($event, row, $index)"
              />
              <span v-else>{{ row.groups?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('askCompleteTime')"
            prop="askCompleteTime"
            label="要求完成日期"
            align="center"
            min-width="130px"
          >
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-date-picker
                v-if="!row.boolIssueEnum"
                v-model="row.askCompleteTime"
                type="date"
                size="mini"
                value-format="x"
                style="width: 100%"
                :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
                :placeholder="$index === 0 ? '需求完成日期' : '同上'"
                @change="handleAskCompleteTimeChange($event, row, $index)"
              />
              <span v-else>{{ row.askCompleteTime }}</span>
            </template>
          </el-table-column>
          <!-- <el-table-column v-permission="[...permission.del]" label="操作" width="100px" align="center" fixed="right">
            <template #default="{ row }">
              <udOperation :showEdit="false" :data="row" />
            </template>
          </el-table-column> -->
        </common-table>
        <!--分页组件-->
        <!-- <pagination /> -->
      </template>
      <preview-dialog v-model:visible="previewVisible" :list="submitList" :info="currentNesting" @success="handleIssueSuccess"/>
    </div>
  </div>
</template>

<script setup>
import { getNestingTaskDetail } from '@/api/mes/scheduling-manage/machine-part'
import { ref } from 'vue'
import { ElMessage } from 'element-plus'
import moment from 'moment'

import { componentTypeEnum } from '@enum-ms/mes'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/mes'

import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import pagination from '@crud/Pagination'
// import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import nestingTaskList from './module/nesting-task-list.vue'
import previewDialog from './module/preview-dialog'
import { deepClone } from '@/utils/data-type'

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
    crudApi: { get: getNestingTaskDetail },
    queryOnPresenterCreated: false,
    dataPath: '',
    hasPagination: false,
    requiredQuery: ['id']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })
const nestingTaskRef = ref()

// --------------------------- 获取生产班组 start ------------------------------
const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

async function fetchGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.MACHINE_PART.V })
    groupLoad.value = true
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}
// --------------------------- 获取生产班组 end --------------------------------

const tableRules = {
  groupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }],
  askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}
const ditto = new Map([
  ['groupsId', '同上'],
  ['askCompleteTime', '同上']
])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

CRUD.HOOK.beforeRefresh = async () => {
  await fetchGroups()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data = res.data.map((v, i) => {
    if (i > 0 && !v.boolIssueEnum) {
      v.askCompleteTime = '同上'
      v.groupsId = '同上'
    }
    if (v.boolIssueEnum) {
      v.askCompleteTime = moment(v.askCompleteTime).format('YYYY-MM-DD')
    }
    v.rowKey = i + '' + Math.random()
    v.spec = `${v.thick}*${v.width}*${v.length}`
    return v
  })
}

function handleGroupsChange(val, row, index) {
  if (index !== 0 && !val) {
    row.groupsId = '同上'
  }
}

function handleAskCompleteTimeChange(val, row, index) {
  if (index !== 0 && !val) {
    row.askCompleteTime = '同上'
  }
}

function showPdf(row) {
  if (row.pdfUrl) {
    window.open(row.pdfUrl)
  } else {
    ElMessage.error('文档不存在')
  }
}

function selectable(row, rowIndex) {
  return !row.boolIssueEnum
}

const currentNesting = ref()
const previewVisible = ref(false)
const submitList = ref([])

function handleNestingTaskClick(val, query) {
  currentNesting.value = val
  crud.query.id = val?.id
  if (val?.id) {
    crud.toQuery()
  }
}

async function handleIssueSuccess() {
  crud.data = []
  const _nestingTaskInfo = deepClone(currentNesting.value)
  await nestingTaskRef?.value?.refresh(_nestingTaskInfo)
}

function toBatchIssue() {
  if (!crud.selections) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  submitList.value = []
  const _list = crud.selections.map((v) => v)
  const { validResult, dealList } = tableValidate(_list)
  if (validResult) {
    cleanUpData(dealList)
    submitList.value = dealList.map(v => {
      return {
        ...v,
        ...schedulingGroups.value.obj[v.groupsId]
      }
    })
    previewVisible.value = true
  }
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 480px;
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

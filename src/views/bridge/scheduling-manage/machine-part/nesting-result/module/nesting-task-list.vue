<template>
  <div class="head-container">
    <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择月份"
      style="width: 120px"
      class="filter-item"
      @change="fetchTaskList"
    />
    <common-radio-button
      v-model="query.boolNestCutEnum"
      :options="layOffWayTypeEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="handleBoolNestingChange"
    />
    <common-radio-button
      v-if="query.boolNestCutEnum === layOffWayTypeEnum.NESTING.V"
      v-model="query.issueStatusEnum"
      :options="issueStatusEnum.ENUM"
      :unshowVal="[issueStatusEnum.HAS_ISSUED.V, issueStatusEnum.NOT_NESTING.V]"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="fetchTaskList"
    />
  </div>
  <common-table
    ref="nestingTaskTableRef"
    v-loading="loading"
    :data-format="dataFormat"
    highlight-current-row
    :data="tableData"
    return-source-data
    row-key="id"
    :stripe="false"
    :max-height="maxHeight - 45"
    style="width: 100%"
    @current-change="handleClickChange"
  >
    <el-table-column prop="orderNumber" :show-overflow-tooltip="true" label="任务单号" min-width="100" align="center" />
    <el-table-column :show-overflow-tooltip="true" label="数量（件）" min-width="60" align="center">
      <template #default="{ row }">
        <span>{{ row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" label="套料状态" min-width="60" align="center">
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
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" label="排产状态" min-width="60" align="center">
      <template #default="{ row }">
        <el-tag v-if="row.taskStatusEnum" effect="plain" :type="mesSchedulingStatusEnum.V[row.taskStatusEnum].T">{{
          mesSchedulingStatusEnum.VL[row.taskStatusEnum]
        }}</el-tag>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <el-pagination
    :total="total"
    :current-page="queryPage.pageNumber"
    :page-size="queryPage.pageSize"
    style="margin-top: 8px"
    layout="total, prev, pager, next, sizes"
    @size-change="handleSizeChange"
    @current-change="handleCurrentChange"
  />
</template>

<script setup>
import { getNestingTask } from '@/api/bridge/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose, nextTick } from 'vue'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import moment from 'moment'

import { machinePartSchedulingIssueStatusEnum as issueStatusEnum, mesSchedulingStatusEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/bridge'

import usePagination from '@compos/use-pagination'
import { isNotBlank } from '@/utils/data-type'

const emit = defineEmits(['nesting-task-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const nestingTaskTableRef = ref()
const month = ref(moment().startOf('month').valueOf().toString())
const query = ref({
  issueStatusEnum: issueStatusEnum.IN_NESTING.V
})
const tableData = ref([])
const loading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchTaskList })

fetchTaskList()

async function fetchTaskList(nestingTaskInfo) {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    tableData.value = []
    const { content, totalElements } = await getNestingTask({
      date: month.value,
      ...query.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    tableData.value = content.map((v) => {
      // v.projectId = v.project?.id
      if (nestingTaskInfo && isNotBlank(nestingTaskInfo) && v.id === nestingTaskInfo.id) {
        nextTick(() => {
          nestingTaskTableRef.value?.setCurrentRow(v)
        })
      }
      return v
    })
  } catch (error) {
    console.log('获取可排产的切割任务单错误', error)
  } finally {
    loading.value = false
  }
}

function handleBoolNestingChange() {
  query.value.issueStatusEnum = undefined
  fetchTaskList()
}

function handleClickChange(val) {
  emit('nesting-task-click', val, query)
}

defineExpose({
  refresh: fetchTaskList
})
</script>

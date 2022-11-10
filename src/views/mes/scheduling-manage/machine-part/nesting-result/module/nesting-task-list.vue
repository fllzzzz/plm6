<template>
  <div class="head-container">
    <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择月份"
      style="width: 48%"
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
        <el-tag v-if="row.issueStatusEnum" effect="plain" :type="issueStatusEnum.V[row.issueStatusEnum].T">{{
          issueStatusEnum.VL[row.issueStatusEnum]
        }}</el-tag>
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
import { getNestingTask } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose, nextTick } from 'vue'
import moment from 'moment'

import { machinePartSchedulingIssueStatusEnum as issueStatusEnum, mesSchedulingStatusEnum } from '@enum-ms/mes'

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
const tableData = ref([])
const loading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchTaskList })

fetchTaskList()

async function fetchTaskList(nestingTaskInfo) {
  try {
    loading.value = true
    tableData.value = []
    const { content, totalElements } = await getNestingTask({
      date: month.value,
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

function handleClickChange(val) {
  emit('nesting-task-click', val)
}

defineExpose({
  refresh: fetchTaskList
})
</script>

<template>
  <div class="head-container">
    <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      format="YYYY年MM月"
      :clearable="false"
      placeholder="选择月份"
      style="width: 48%"
      class="filter-item"
      @change="fetchTaskList"
    />
  </div>
  <common-table
    v-loading="loading"
    :data-format="dataFormat"
    highlight-current-row
    :data="tableData"
    :stripe="false"
    :max-height="maxHeight"
    style="width: 100%"
    @current-change="handleCurrentChange"
  >
    <el-table-column prop="orderNumber" :show-overflow-tooltip="true" label="任务单号" min-width="100" align="center" />
    <el-table-column :show-overflow-tooltip="true" label="数量（件）" min-width="60" align="center">
      <template #default="{ row }">
        <span>{{ row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" label="套料状态" min-width="60" align="center">
      <template #default="{ row }">
        <span>{{ row.issueStatusEnum }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" label="排产状态" min-width="60" align="center">
      <template #default="{ row }">
        <span>{{ row.issueStatusEnum }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { getNestingTask } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits } from 'vue'
import moment from 'moment'

const emit = defineEmits(['nesting-task-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const month = ref(moment().valueOf().toString())
const tableData = ref([])
const loading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

fetchTaskList()

async function fetchTaskList() {
  try {
    loading.value = true
    tableData.value = []
    const { content } = await getNestingTask({
      // date: month.value
    })
    tableData.value = content.map((v) => {
      // v.projectId = v.project?.id
      return v
    })
  } catch (error) {
    console.log('获取可排产的切割任务单错误', error)
  } finally {
    loading.value = false
  }
}

function handleCurrentChange(val) {
  emit('nesting-task-click', val)
}
</script>

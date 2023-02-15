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
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="projectName" :show-overflow-tooltip="true" label="项目列表" min-width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.serialNumber }}-{{ row.name }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { getProjectTaskDetail } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose } from 'vue'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['nesting-task-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const nestingTaskTableRef = ref()
const month = ref(moment().startOf('month').valueOf().toString())
const query = ref({})
const tableData = ref([])
const loading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

fetchTaskList()

async function fetchTaskList() {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    tableData.value = []
    const { content } = await getProjectTaskDetail({
      date: month.value
    })
    tableData.value = content || []
  } catch (error) {
    console.log('获取排产项目列表错误', error)
  } finally {
    loading.value = false
  }
}

function handleClickChange(val) {
  emit('nesting-task-click', val, query)
}

defineExpose({
  refresh: fetchTaskList
})
</script>

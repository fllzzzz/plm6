<template>
  <div class="head-container">
    <!-- <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择月份"
      style="width: 120px"
      class="filter-item"
      @change="fetchTaskList"
    /> -->
    <el-date-picker
      v-model="date"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px"
      class="filter-item"
      @change="handleDateChange"
    />
    <el-input
      v-model="name"
      placeholder="项目搜索"
      class="filter-item"
      style="width: 160px"
      size="small"
      clearable
      @keyup.enter="fetchTaskList"
    />
    <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
    <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
      重置
    </common-button>
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
import { ref, defineProps, defineEmits, defineExpose, watch, inject } from 'vue'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['nesting-task-click'])
const startDate = ref(moment().startOf('year').valueOf())
const endDate = ref(moment().valueOf())
const date = ref([moment().startOf('year').valueOf(), moment().valueOf()])
const name = ref()

const crud = inject('crud')
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

watch([() => month.value, () => name.value], () => {
  crud.data = []
  crud.query.projectId = undefined
})

fetchTaskList()

async function fetchTaskList() {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    tableData.value = []
    const { content } = await getProjectTaskDetail({
      startDate: startDate.value,
      endDate: endDate.value,
      name: name.value
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

function handleDateChange(val) {
  if (val && val.length > 1) {
    startDate.value = val[0]
    endDate.value = val[1]
  } else {
    startDate.value = moment().startOf('year').valueOf()
    endDate.value = moment().valueOf()
  }
  fetchTaskList()
}

function searchQuery() {
  fetchTaskList()
}

function resetQuery() {
  name.value = undefined
  date.value = []
  startDate.value = undefined
  endDate.value = undefined
  fetchTaskList()
}

defineExpose({
  refresh: fetchTaskList
})
</script>

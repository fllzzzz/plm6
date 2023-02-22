<template>
  <div class="head-container">
    <el-date-picker
      v-model="date"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px; margin-right: 10px"
      class="filter-item date-item"
      @change="handleDateChange"
    />
    <workshop-select
      v-model="workshopId"
      placeholder="请选择车间"
      :factory-id="factoryId"
      style="width: 200px"
      class="filter-item"
      defaultValue
      @change="fetchProcessList"
    />
    <!-- <process-radio-button v-model="processId" default size="small" class="filter-item" /> -->
  </div>
  <common-table
    v-loading="loading"
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
    <el-table-column prop="process.name" :show-overflow-tooltip="true" label="工序" min-width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.process?.name }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="groups.name" :show-overflow-tooltip="true" label="班组" min-width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.workshop?.name }}-{{ row.groups?.name }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="产量（件/kg）" min-width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.inspectionQuantity }}/{{ row.netWeight }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { getProcessList } from '@/api/mes/factory-report/group-report.js'
import { ref, defineProps, defineEmits, watch, inject } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import moment from 'moment'
import workshopSelect from '@comp-mes/workshop-select'
// import processRadioButton from '@comp-mes/process-radio-button'
import checkPermission from '@/utils/system/check-permission'
import { machinePartSchedulingNestingResultPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['nesting-task-click'])
const crud = inject('crud')
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])
// const year = ref(moment().valueOf().toString())
const serialNumber = ref()
const startDate = ref()
const endDate = ref()
// const processId = ref()
const workshopId = ref()
const factoryId = ref()
const query = ref({})
const tableData = ref([])
const loading = ref(false)

watch([() => startDate.value, () => endDate.value, () => serialNumber.value], () => {
  crud.data = []
  crud.query.projectId = undefined
})

fetchProcessList()

async function fetchProcessList() {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    tableData.value = []
    const data = await getProcessList({
      startDate: startDate.value,
      endDate: endDate.value,
      serialNumber: serialNumber.value
    })
    tableData.value = data || []
  } catch (error) {
    console.log('获取构件项目列表错误', error)
  } finally {
    loading.value = false
  }
}

// 时间变动
function handleDateChange(val) {
  if (val && val.length > 1) {
    startDate.value = val[0]
    endDate.value = val[1]
  } else {
    startDate.value = undefined
    endDate.value = undefined
  }
  fetchProcessList()
}

function handleClickChange(val) {
  emit('nesting-task-click', val, query)
}
</script>

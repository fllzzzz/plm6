<template>
  <div class="head-container">
    <el-date-picker
      v-model="year"
      type="year"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择年份"
      style="width: 100px"
      class="filter-item"
      @change="fetchProjectList"
    />
    <div>
      <el-input
        v-model="serialNumber"
        size="small"
        placeholder="项目搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="fetchProjectList"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchClick">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetClick">
        重置
      </common-button>
    </div>
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
    <el-table-column prop="projectName" :show-overflow-tooltip="true" label="项目列表" min-width="100" align="center">
      <template #default="{ row }">
        <span style="cursor: pointer">{{ row.serialNumber }}-{{ row.name }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { getProjectInfo } from '@/api/mes/work-order-manage/machine-part.js'
import { ref, defineProps, defineEmits, watch, inject } from 'vue'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { machinePartWorkOrderPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['nesting-task-click'])
const crud = inject('crud')
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const year = ref(moment().valueOf().toString())
const serialNumber = ref()
const query = ref({})
const tableData = ref([])
const loading = ref(false)

watch([() => year.value, () => serialNumber.value], () => {
  crud.data = []
  crud.query.projectId = undefined
})

fetchProjectList()

async function fetchProjectList() {
  if (!checkPermission(permission.get)) return
  try {
    loading.value = true
    tableData.value = []
    const data = await getProjectInfo({
      localDateTime: year.value ? year.value : moment().valueOf().toString(),
      serialNumber: serialNumber.value,
      processType: mesMachinePartOrderTypeEnum.CUTTING_ORDER.V
    })
    tableData.value = data || []
  } catch (error) {
    console.log('获取构件项目列表错误', error)
  } finally {
    loading.value = false
  }
}

// 搜索
function searchClick() {
  fetchProjectList()
}

// 重置
function resetClick() {
  year.value = undefined
  serialNumber.value = undefined
  fetchProjectList()
}

function handleClickChange(val) {
  emit('nesting-task-click', val, query)
}
</script>

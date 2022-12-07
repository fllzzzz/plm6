<template>
  <div class="head-container">
    <!-- <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      format="YYYY年MM月"
      :clearable="false"
      placeholder="选择月份"
      style="width: 48%"
      class="filter-item"
      @change="fetchTime(null)"
    /> -->
    <project-header-time v-model="month" multiple :data="timeList" @change="fetchProject" empty-text="暂无零件排产信息" />
    <!-- <common-select
      v-loading="timeLoading"
      v-model="month"
      :options="timeList"
      :loading="timeLoading"
      loading-text="加载中"
      clearable
      multiple
      default
      style="width: 48%"
      class="filter-item"
      :placeholder="timeLoading ? '加载中' : '选择月份'"
      :dataStructure="{ key: 'timeStamp', label: 'month', value: 'timeStamp' }"
      @change="fetchProject"
    /> -->
  </div>
  <common-table
    ref="projectTableRef"
    v-loading="projectLoading"
    :data-format="dataFormat"
    :data="tableData"
    :stripe="false"
    :max-height="maxHeight"
    style="width: 100%"
    @selection-change="handleSelectionChange"
  >
    <el-table-column type="selection" width="45" align="center" />
    <el-table-column prop="project" :show-overflow-tooltip="true" label="项目" min-width="100" />
    <el-table-column :show-overflow-tooltip="true" label="零件量（件|kg）" min-width="60" align="center">
      <template #default="{ row }">
        <span>{{ row.quantity }} | {{ row.totalNetWeight }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { getProject, getMonth } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose, nextTick } from 'vue'
import { isBlank, isNotBlank } from '@/utils/data-type'
import moment from 'moment'

import checkPermission from '@/utils/system/check-permission'
import { machinePartSchedulingPM as permission } from '@/page-permission/mes'

import projectHeaderTime from '@/views/mes/scheduling-manage/common/project-header-time.vue'

const emit = defineEmits(['project-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const timeList = ref([])
const timeLoading = ref(false)
const projectTableRef = ref()
// const date = ref()
const month = ref([])
const tableData = ref([])
const projectLoading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

fetchTime()

async function fetchTime(lastQuery) {
  if (!checkPermission(permission.get)) return
  try {
    timeList.value = []
    tableData.value = []
    month.value = []
    timeLoading.value = true
    const { content } = await getMonth()
    timeList.value = content.map((v) => {
      const timeStamp = moment(v, 'YYYY-MM').valueOf()
      const _arr = v.split('-')
      return {
        timeStamp,
        year: _arr[0],
        month: _arr[1]
      }
    })
    // if (timeList.value?.length) {
    //   date.value = timeList.value[0].dateTime
    //   fetchProject()
    // }
    if (lastQuery && isNotBlank(lastQuery.monthList) && timeList.value?.length) {
      for (let i = 0; i < lastQuery.monthList.length; i++) {
        const m = lastQuery.monthList[i]
        if (timeList.value?.findIndex(v => v.timeStamp === m) !== -1) {
          month.value.push(m)
        }
      }
    }
    if (isBlank(month.value) && timeList.value?.length) {
      const curTime = moment().startOf('month').valueOf()
      const curIndex = timeList.value?.findIndex(v => v.timeStamp === curTime)
      if (curIndex !== -1) {
        month.value.push(curTime)
      } else {
        month.value = [timeList.value[0].timeStamp]
      }
    }
    await fetchProject(lastQuery)
  } catch (error) {
    console.log('获取排程信息时间错误', error)
  } finally {
    timeLoading.value = false
  }
}

// function disabledDate(time) {
//   return timeList.value?.indexOf(moment(time).valueOf()) === -1 && moment(time).month() === month.value
// }

async function fetchProject(lastQuery) {
  tableData.value = []
  if (!checkPermission(permission.get) || isBlank(month.value)) return
  try {
    projectLoading.value = true
    const { content } = await getProject({
      // dateTime: date.value,
      monthList: month.value
    })
    const needSelectIndex = []
    tableData.value = content.map((v, index) => {
      v.projectId = v.project?.id
      if (lastQuery && lastQuery?.projectIds.indexOf(v.projectId) !== -1) {
        needSelectIndex.push(index)
      }
      return v
    })
    needSelectIndex.forEach(i => {
      nextTick(() => {
        projectTableRef.value?.toggleRowSelection(tableData.value[i], true)
      })
    })
  } catch (error) {
    console.log('获取排程信息，项目树错误', error)
  } finally {
    projectLoading.value = false
  }
}

function handleSelectionChange(val) {
  emit('project-click', val, month.value)
}

defineExpose({
  refresh: fetchTime
})
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>

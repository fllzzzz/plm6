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
      @change="fetchTime"
    />
    <common-select
      v-model="date"
      :options="timeList"
      :clearable="false"
      style="width: 48%"
      class="filter-item"
      placeholder="选择日期"
      :dataStructure="{ key: 'dateTime', label: 'date', value: 'dateTime' }"
      @change="fetchProject"
    />
  </div>
  <common-table
    ref="projectTableRef"
    v-loading="loading"
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
import { getProject, getDate } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose } from 'vue'
import moment from 'moment'

const emit = defineEmits(['project-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const timeList = ref([])
const projectTableRef = ref()
const date = ref()
const month = ref(moment().valueOf().toString())
const tableData = ref([])
const loading = ref(false)
const dataFormat = ref([['project', 'parse-project']])

fetchTime()

async function fetchTime() {
  try {
    timeList.value = []
    tableData.value = []
    date.value = undefined
    const { content } = await getDate({
      dateTime: month.value
    })
    timeList.value = content.map((v) => {
      const _dateTime = moment(v, 'YYYY/MM/DD').valueOf()
      return {
        dateTime: _dateTime,
        date: moment(_dateTime).date() + '日'
      }
    })
    if (timeList.value?.length) {
      date.value = timeList.value[0].dateTime
      fetchProject()
    }
  } catch (error) {
    console.log('获取排程信息时间错误', error)
  }
}

// function disabledDate(time) {
//   return timeList.value?.indexOf(moment(time).valueOf()) === -1 && moment(time).month() === month.value
// }

async function fetchProject() {
  try {
    loading.value = true
    tableData.value = []
    const { content } = await getProject({
      dateTime: date.value
    })
    tableData.value = content.map((v) => {
      v.projectId = v.project?.id
      return v
    })
  } catch (error) {
    console.log('获取排程信息，项目树错误', error)
  } finally {
    loading.value = false
  }
}

function handleSelectionChange(val) {
  emit('project-click', val, date.value)
}

defineExpose({
  artifactDateTime: date
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

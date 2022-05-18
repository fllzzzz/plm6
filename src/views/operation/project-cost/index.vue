<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="fetchList"
      />
      <div style="float: right; font-size: 16px; color: #303133">单位：万元</div>
    </div>
    <common-table v-loading="loading" :data="tableData" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="120">
        <template #default="{ row }">
          <span class="project-name">{{ projectNameFormatter(row.project, null, false) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="type" :show-overflow-tooltip="true" label="订单类型" >
        <template #default="{ row }">
          <span>{{ row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="type" :show-overflow-tooltip="true" label="状态">
        <template #default="{ row }">
          <span>{{ row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="type" :show-overflow-tooltip="true" label="收入">
        <template #default="{ row }">
          <span>{{ row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="type" :show-overflow-tooltip="true" label="支出">
        <template #default="{ row }">
          <span>{{ row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="type" :show-overflow-tooltip="true" label="预估利润">
        <template #default="{ row }">
          <span>{{ row.type }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { getProjectCost } from '@/api/operation/project-cost'
import { onMounted, ref } from 'vue'
import moment from 'moment'

import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'

const { maxHeight } = useMaxHeight()

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())
const loading = ref(false)
const tableData = ref([])

onMounted(() => {
  fetchList()
})

async function fetchList() {
  try {
    loading.value = true
    const { content } = await getProjectCost({ dateTime: year.value })
    tableData.value = content
  } catch (error) {
    console.log(error, '获取项目直接成本')
  } finally {
    loading.value = false
  }
}
</script>

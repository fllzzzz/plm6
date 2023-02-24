<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 100px"
        placeholder="选择年"
        class="filter-item"
        format="YYYY"
        value-format="YYYY"
        clearable
        :disabled-date="disabledDate"
        @change="fetchManageFee"
      />

      <export-button v-permission="permission.download" class="filter-item" :fn="getManageFeeListFn" :params="{ year: year }"> 管理费清单 </export-button>
    </div>
    <common-table ref="tableRef" :data="manageFeeList" :empty-text="checkPermission(permission.get)?'暂无数据':'暂无权限'" :max-height="maxHeight" row-key="id" style="width: 100%">
      <el-table-column label="月份" prop="month" align="center" width="60px" />
      <el-table-column label="月产（吨）" prop="monthCapacity" align="center">
        <template #default="{ row }">
          <span>{{ convertUnits(row.monthCapacity, 'kg', 't', 2) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="管理人员数量" prop="managementUserCount" align="center" />
      <el-table-column label="应发工资（元）" prop="managementUserMoney" align="center" />
      <el-table-column label="报销（元）" prop="reimbursementMoney" align="center" />
      <el-table-column label="其他（元）" prop="otherMoney" align="center" />
      <el-table-column label="合计金额（元）" prop="sumMoney" align="center" />
      <el-table-column label="平均单价（元/吨）" prop="avgMoney" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { getManageFee, getManageFeeListFn } from '@/api/operation/management-fee'

import checkPermission from '@/utils/system/check-permission'
import { operationManagementPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { convertUnits } from '@/utils/convert/unit'

import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const tableRef = ref()
const manageFeeList = ref([])

onMounted(() => {
  fetchManageFee()
})
async function fetchManageFee() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    const { content } = await getManageFee({
      year: year.value
    })
    manageFeeList.value = content || []
  } catch (error) {
    console.log('获取管理费列表失败', error)
  }
}
function disabledDate(time) {
  return time > new Date()
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

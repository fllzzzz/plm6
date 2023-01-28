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
        @change="fetchWaterElectric"
      />
      <export-button v-permission="permission.download" class="filter-item" :fn="getWaterElectricListFn" :params="{ year: year }">
        水电费清单
      </export-button>
    </div>
    <common-table
      ref="tableRef"
      :data="waterElectricList"
      :empty-text="checkPermission(permission.get) ? '暂无数据' : '暂无权限'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column label="月份" prop="month" align="center" />
      <el-table-column label="月产（吨）" prop="monthOutput" align="center">
        <template #default="{ row }">
          <span>{{ convertUnits(row.monthOutput, 'kg', 't', DP.COM_WT__T) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="用电度数" prop="usedElectricityMete" align="center" />
      <el-table-column label="均价（元/度）" prop="ElectricityAverageUnitPrice" align="center">
        <template #default="{ row }">
          <span>{{ row.usedElectricityMete ? (row.electricityPrice / row.usedElectricityMete).toFixed(2) : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column label="电费（元）" prop="electricityPrice" align="center" />
      <el-table-column label="折合电费（元/吨）" prop="equivalentElectricity" align="center">
        <template #default="{ row }">
          <span>{{
            row.monthOutput
              ? (row.electricityPrice / convertUnits(row.monthOutput, 'kg', 't', DP.COM_WT__T)).toFixed(2)
              : row.electricityPrice
          }}</span>
        </template>
      </el-table-column>
      <el-table-column label="用水量（吨）" prop="usedWaterMete" align="center" />
      <el-table-column label="均价（元/吨）" prop="waterAverageUnitPrice" align="center">
        <template #default="{ row }">
          <span>{{ row.usedWaterMete ? (row.waterPrice / row.usedWaterMete).toFixed(2) : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column label="水费（元）" prop="waterPrice" align="center" />
      <el-table-column label="折合水费（元/吨）" prop="equivalentWater" align="center">
        <template #default="{ row }">
          <span>{{
            row.monthOutput ? (row.waterPrice / convertUnits(row.monthOutput, 'kg', 't', DP.COM_WT__T)).toFixed(2) : row.waterPrice
          }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { getWaterElectric, getWaterElectricListFn } from '@/api/operation/water-electricity-fee'

import checkPermission from '@/utils/system/check-permission'
import { operationWaterElectricityPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { parseTime } from '@/utils/date'

import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const tableRef = ref()
const waterElectricList = ref([])

onMounted(() => {
  fetchWaterElectric()
})

async function fetchWaterElectric() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    const { content } = await getWaterElectric({
      year: year.value
    })
    waterElectricList.value = content || []
  } catch (error) {
    console.log('获取水电费列表失败', error)
  }
}

function disabledDate(time) {
  return time > new Date()
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 3) {
      sums[index] = 0
      const usedElectricityMeteList = data.map((v) => v.usedElectricityMete)
      const electricityPriceList = data.map((v) => v.electricityPrice)
      const usedElectricityMeteSum = usedElectricityMeteList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const electricityPriceSum = electricityPriceList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = usedElectricityMeteSum ? (electricityPriceSum / usedElectricityMeteSum).toFixed(2) : electricityPriceSum.toFixed(2)
      return
    }
    if (index === 5) {
      sums[index] = 0
      const monthOutputList = data.map((v) => v.monthOutput)
      const electricityPriceList = data.map((v) => v.electricityPrice)
      const monthOutputSum = monthOutputList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const electricityPriceSum = electricityPriceList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = monthOutputSum ? (electricityPriceSum / (monthOutputSum / 1000)).toFixed(2) : electricityPriceSum.toFixed(2)
      return
    }
    if (index === 7) {
      sums[index] = 0
      const usedWaterMeteList = data.map((v) => v.usedWaterMete)
      const waterPriceList = data.map((v) => v.waterPrice)
      const usedWaterMeteSum = usedWaterMeteList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const waterPriceSum = waterPriceList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = usedWaterMeteSum ? (waterPriceSum / usedWaterMeteSum).toFixed(2) : waterPriceSum.toFixed(2)
      return
    }
    if (index === 9) {
      sums[index] = 0
      const monthOutputList = data.map((v) => v.monthOutput)
      const waterPriceList = data.map((v) => v.waterPrice)
      const monthOutputSum = monthOutputList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const waterPriceSum = waterPriceList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = monthOutputSum ? (waterPriceSum / (monthOutputSum / 1000)).toFixed(2) : waterPriceSum.toFixed(2)
      return
    }
    if (
      column.property === 'monthOutput' ||
      column.property === 'usedElectricityMete' ||
      column.property === 'electricityPrice' ||
      column.property === 'usedWaterMete' ||
      column.property === 'waterPrice'
    ) {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum.toFixed(2)
    }

    if (index === 1) {
      sums[index] = convertUnits(sums[index], 'kg', 't', DP.COM_WT__T)
    }
  })
  return sums
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

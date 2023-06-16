<template>
  <div class="app-container">
    <div class="head-container">
      <div style="float: left">
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
          @change="fetchPaintingFee"
        />
      </div>
      <div style="float: right">
        <excel-export-button
          v-permission="permission.download"
          :btn-name="`涂装费清单`"
          :btn-type="'warning'"
          :template="paintingFeeListETmpl"
          :filename="`涂装费清单`"
          :params="{ year: year }"
        />
      </div>
    </div>
    <common-table
      ref="tableRef"
      :data="paintingList"
      :empty-text="checkPermission(permission.get) ? '暂无数据' : '暂无权限'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column type="index" label="序号" prop="indx" align="center" />
      <el-table-column label="项目名称" prop="project.name" align="center" min-width="160px" />
      <el-table-column label="累计产量（吨）" prop="mete" align="center">
        <template #default="{ row }">
          <span>{{ row.mete ? convertUnits(row.mete, 'kg', 't', 2) : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column label="油漆面积（㎡）" prop="area" align="center">
        <template v-slot="scope">
          <span>{{ convertUnits(scope.row.area, 'mm2', 'm2', DP.COM_AREA__M2) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="涂装单价" prop="paintingUnitPrice" align="center" />
      <el-table-column label="应付工程款（元）" prop="price" align="center" />
      <el-table-column label="平均单价（元/吨）" prop="averageUnitPrice" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { getPaintingFee } from '@/api/operation/painting-fee'

import checkPermission from '@/utils/system/check-permission'
import { paintingFeeAnalysisPM as permission } from '@/page-permission/operation'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import useMaxHeight from '@compos/use-max-height'

import paintingFeeListETmpl from '@/utils/excel/export-template/operation/painting-fee-list'
import ExcelExportButton from '@comp-common/excel-export-button/index.vue'
// import useDecimalPrecision from '@compos/store/use-decimal-precision'

// const { decimalPrecision } = useDecimalPrecision()

const year = ref(parseTime(new Date(), '{y}'))
const paintingList = ref([])

onMounted(() => {
  fetchPaintingFee()
})
async function fetchPaintingFee() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    const { content } = await getPaintingFee({
      year: year.value
    })
    content.forEach((v) => {
      v.paintingUnitPrice = v.price && v.area ? (v.price / convertUnits(v.area, 'mm2', 'm2', DP.COM_AREA__M2)).toFixed(2) : 0
      v.averageUnitPrice = (v.price / convertUnits(v.mete, 'kg', 't', DP.COM_WT__T)).toFixed(2)
    })
    paintingList.value = content || []
  } catch (error) {
    console.log('获取涂装费列表失败', error)
  }
}

function disabledDate(time) {
  return time > new Date()
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 6) {
      const meteList = data.map((item) => item.mete)
      const priceList = data.map((item) => item.price)
      const meteSum = meteList.reduce((prev, curr) => {
        if (Number(curr)) {
          return prev + curr
        } else {
          return prev
        }
      }, 0)
      const priceSum = priceList.reduce((prev, curr) => {
        if (Number(curr)) {
          return prev + curr
        } else {
          return prev
        }
      }, 0)
      sums[index] = meteSum ? (priceSum / (meteSum / 1000)).toFixed(2) : 0
    }
    if (column.property === 'mete') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = convertUnits(sums[index], 'kg', 't', 2)
      }
    }
    if (column.property === 'area') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = convertUnits(sums[index], 'mm2', 'm2', DP.COM_AREA__M2)
      }
    }
    if (column.property === 'price') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index].toFixed(2)
      }
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
</style>

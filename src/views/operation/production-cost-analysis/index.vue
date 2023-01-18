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
        @change="fetchProductAnalysis"
      />
      <!-- <excel-export-button
          class="filter-item"
          v-permission="permission.download"
          :btn-name="`生产成本分析清单`"
          :btn-type="'warning'"
          :template="paintingFeeListETmpl"
          :filename="`生产成本分析清单`"
          :params="{ year: year }"
        /> -->
      <!-- <export-button class="filter-item" v-permission="permission.download" :fn="getProductAnalysisPrint" :params="{ year: year }"> 生产成本分析清单 </export-button> -->
    </div>
    <common-table
      ref="tableRef"
      :data="productionList"
      :empty-text="checkPermission(permission.get)?'暂无数据':'暂无权限'"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      row-key="id"
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column label="月份" prop="month" width="70" align="center" />
      <el-table-column label="月产（吨）" prop="productionMete" align="center">
        <template #default="{ row }">
          <span>{{ convertUnits(row.productionMete, 'kg', 't', DP.COM_WT__T) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="人工费" prop="laborFee" align="center" />
      <el-table-column label="辅材费" prop="auxiliaryFee" align="center" />
      <el-table-column label="气体" prop="gasFee" align="center" />
      <el-table-column label="水电费" prop="waterElectricityFee" align="center" />
      <el-table-column label="厂房折旧" prop="plantDepreciationFee" align="center" />
      <el-table-column label="设备折旧" prop="deviceDepreciationFee" align="center" />
      <el-table-column label="检测费" prop="testingFee" align="center" />
      <el-table-column label="管理费" prop="managementFee" align="center" />
      <el-table-column label="总额" prop="totalFee" align="center" />
      <el-table-column label="平均单价" prop="avgProductionFee" align="center" />
      <el-table-column label="考勤人数" prop="attendanceQuantity" width="70" align="center" />
      <el-table-column label="人均产量" prop="avgWorkerProductionMete" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { getProductAnalysis } from '@/api/operation/production-cost-analysis'

import checkPermission from '@/utils/system/check-permission'
import { productionCostAnalysisPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

const columnsDataFormat = [
  ['laborFee', ['to-fixed', 2]],
  ['auxiliaryFee', ['to-fixed', 2]],
  ['gasFee', ['to-fixed', 2]],
  ['waterElectricityFee', ['to-fixed', 2]],
  ['plantDepreciationFee', ['to-fixed', 2]],
  ['deviceDepreciationFee', ['to-fixed', 2]],
  ['testingFee', ['to-fixed', 2]],
  ['managementFee', ['to-fixed', 2]],
  ['totalFee', ['to-fixed', 2]],
  ['avgProductionFee', ['to-fixed', 2]]
]

// import ExportButton from '@comp-common/export-button/index.vue'
// import ExcelExportButton from '@comp-common/excel-export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const tableRef = ref()
const productionList = ref([])

onMounted(() => {
  fetchProductAnalysis()
})

async function fetchProductAnalysis() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    const { content } = await getProductAnalysis({
      year: year.value
    })
    const laborFeeList = content.map((v) => v.laborFee)
    const auxiliaryFeeList = content.map((v) => v.auxiliaryFee)
    const gasFeeList = content.map((v) => v.gasFee)
    const waterElectricityFeeList = content.map((v) => v.waterElectricityFee)
    const plantDepreciationFeeList = content.map((v) => v.plantDepreciationFee)
    const deviceDepreciationFeeList = content.map((v) => v.deviceDepreciationFee)
    const testingFeeList = content.map((v) => v.testingFee)
    const managementFeeList = content.map((v) => v.managementFee)
    const productList = content.map((v) => convertUnits(v.productionMete, 'kg', 't', DP.COM_WT__T))
    const laborFeeSum = laborFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const auxiliaryFeeSum = auxiliaryFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const gasFeeSum = gasFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const waterElectricityFeeSum = waterElectricityFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const plantDepreciationFeeSum = plantDepreciationFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const deviceDepreciationFeeSum = deviceDepreciationFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const testingFeeSum = testingFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const managementFeeSum = managementFeeList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    const productSum = productList.reduce((pre, cur) => {
      if (cur) {
        return pre + Number(cur)
      } else {
        return pre
      }
    }, 0)
    content.push(
      {
        month: '合计',
        productionMete: (productSum * 1000).toFixed(DP.COM_WT__T),
        laborFee: laborFeeSum.toFixed(2),
        auxiliaryFee: auxiliaryFeeSum.toFixed(2),
        gasFee: gasFeeSum.toFixed(2),
        waterElectricityFee: waterElectricityFeeSum.toFixed(2),
        plantDepreciationFee: plantDepreciationFeeSum.toFixed(2),
        deviceDepreciationFee: deviceDepreciationFeeSum.toFixed(2),
        testingFee: testingFeeSum.toFixed(2),
        managementFee: managementFeeSum.toFixed(2)
      },
      {
        month: '单项成本',
        laborFee: productSum ? (laborFeeSum / productSum).toFixed(2) : laborFeeSum,
        auxiliaryFee: productSum ? (auxiliaryFeeSum / productSum).toFixed(2) : auxiliaryFeeSum,
        gasFee: productSum ? (gasFeeSum / productSum).toFixed(2) : gasFeeSum,
        waterElectricityFee: productSum ? (waterElectricityFeeSum / productSum).toFixed(2) : waterElectricityFeeSum,
        plantDepreciationFee: productSum ? (plantDepreciationFeeSum / productSum).toFixed(2) : plantDepreciationFeeSum,
        deviceDepreciationFee: productSum ? (deviceDepreciationFeeSum / productSum).toFixed(2) : deviceDepreciationFeeSum,
        testingFee: productSum ? (testingFeeSum / productSum).toFixed(2) : testingFeeSum,
        managementFee: productSum ? (managementFeeSum / productSum).toFixed(2) : managementFeeSum
      },
      {
        month: '占比',
        laborFee: productSum && laborFeeSum ? ((laborFeeSum / productSum / laborFeeSum) * 100).toFixed(2) + '%' : 0,
        auxiliaryFee: productSum && auxiliaryFeeSum ? ((auxiliaryFeeSum / productSum / auxiliaryFeeSum) * 100).toFixed(2) + '%' : 0,
        gasFee: productSum && gasFeeSum ? ((gasFeeSum / productSum / gasFeeSum) * 100).toFixed(2) + '%' : 0,
        waterElectricityFee:
          productSum && waterElectricityFeeSum
            ? ((waterElectricityFeeSum / productSum / waterElectricityFeeSum) * 100).toFixed(2) + '%'
            : 0,
        plantDepreciationFee:
          productSum && plantDepreciationFeeSum
            ? ((plantDepreciationFeeSum / productSum / plantDepreciationFeeSum) * 100).toFixed(2) + '%'
            : 0,
        deviceDepreciationFee:
          productSum && deviceDepreciationFeeSum
            ? ((deviceDepreciationFeeSum / productSum / deviceDepreciationFeeSum) * 100).toFixed(2) + '%'
            : 0,
        testingFee: productSum && testingFeeSum ? ((testingFeeSum / productSum / testingFeeSum) * 100).toFixed(2) + '%' : 0,
        managementFee: productSum && managementFeeSum ? ((managementFeeSum / productSum / managementFeeSum) * 100).toFixed(2) + '%' : 0
      }
    )
    productionList.value = content || []
  } catch (error) {
    console.log('获取生产成本分析失败', error)
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

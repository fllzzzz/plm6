<template>
  <div class="app-container">
    <common-table
      ref="tableRef"
      :data="tableData"
      :max-height="maxHeight"
      style="width: 100%"
      return-source-data
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="项目" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}-{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="构件编号" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="nuclear" prop="nuclear" :show-overflow-tooltip="true" label="核算单位" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.nuclear }}</span>
        </template>
      </el-table-column>
      <el-table-column key="primer" prop="primer" :show-overflow-tooltip="true" label="底漆" align="center">
        <el-table-column key="bottomArea" prop="bottomArea" :show-overflow-tooltip="true" label="面积" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.bottomArea ? toThousand(scope.row.bottomArea) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="bottomTotalPrice" prop="bottomTotalPrice" :show-overflow-tooltip="true" label="金额（元）" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.bottomTotalPrice ? toThousand(scope.row.bottomTotalPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="intermediatePaint" prop="intermediatePaint" :show-overflow-tooltip="true" label="中间漆" align="center">
        <el-table-column key="middleArea" prop="middleArea" :show-overflow-tooltip="true" label="面积" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.middleArea ? toThousand(scope.row.middleArea) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="middleTotalPrice" prop="middleTotalPrice" :show-overflow-tooltip="true" label="金额（元）" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.middleTotalPrice ? toThousand(scope.row.middleTotalPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="topCoat" prop="topCoat" :show-overflow-tooltip="true" label="面漆" align="center">
        <el-table-column key="topArea" prop="topArea" :show-overflow-tooltip="true" label="面积" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.topArea ? toThousand(scope.row.topArea) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="topTotalPrice" prop="topTotalPrice" :show-overflow-tooltip="true" label="金额（元）" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.topTotalPrice ? toThousand(scope.row.topTotalPrice,decimalPrecision.mes) : '/' }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="totalSum" prop="totalSum" :show-overflow-tooltip="true" label="合计" align="center">
        <template v-slot="scope">
          <span>{{
            scope.row.bottomTotalPrice + scope.row.middleTotalPrice + scope.row.topTotalPrice
              ? toThousand((scope.row.bottomTotalPrice + scope.row.middleTotalPrice + scope.row.topTotalPrice),decimalPrecision.mes)
              : '/'
          }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import useMaxHeight from '@compos/use-max-height'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const tableData = [
  {
    projectNumber: '11111',
    projectName: '项目',
    serialNumber: 'GL-3',
    specification: 'BH400*200*6*9',
    interfaceType: '>400<500',
    nuclear: '米',
    bottomArea: 123,
    bottomTotalPrice: 2345,
    middleArea: 111,
    middleTotalPrice: 1333,
    topArea: 123,
    topTotalPrice: 5858,
    totalMete: 200,
    unitPrice: 10,
    totalPrice: 200,
    productionDate: 1654300000
  }
]
const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['bottomArea', 'bottomTotalPrice', 'middleArea', 'middleTotalPrice', 'topArea', 'topTotalPrice']
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

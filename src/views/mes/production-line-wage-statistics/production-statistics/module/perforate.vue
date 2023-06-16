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
    <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column key="interfaceSpec" prop="interfaceSpec" :show-overflow-tooltip="true" label="截面类型" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.interfaceSpec }}</span>
      </template>
    </el-table-column>
    <el-table-column key="aperture" prop="aperture" :show-overflow-tooltip="true" label="孔径" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.aperture }}</span>
      </template>
    </el-table-column>
    <el-table-column key="apertureQuantity" prop="apertureQuantity" :show-overflow-tooltip="true" label="孔数量" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.apertureQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column key="nuclear" prop="nuclear" :show-overflow-tooltip="true" label="核算单位" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.nuclear }}</span>
      </template>
    </el-table-column>
    <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice,decimalPrecision.mes) : 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="总额（元）" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice,decimalPrecision.mes) : 0 }}</span>
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
    apertureQuantity: 12,
    totalMete: 200,
    unitPrice: 10,
    totalPrice: '200',
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
    props: ['apertureQuantity', ['totalPrice', decimalPrecision.mes]]
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

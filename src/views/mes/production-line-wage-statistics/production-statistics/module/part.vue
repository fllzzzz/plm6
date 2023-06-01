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
    <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="项目">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}-{{ scope.row.projectName }}</span>
      </template>
    </el-table-column>
    <el-table-column key="cutType" prop="cutType" :show-overflow-tooltip="true" label="切割方式" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.cutType }}</span>
      </template>
    </el-table-column>
    <el-table-column key="materialType" prop="materialType" :show-overflow-tooltip="true" label="原材料类型" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.materialType }}</span>
      </template>
    </el-table-column>
    <el-table-column key="interfaceSpec" prop="interfaceSpec" :show-overflow-tooltip="true" label="截面规格" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.interfaceSpec }}</span>
      </template>
    </el-table-column>
    <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.specification }}</span>
      </template>
    </el-table-column>
    <el-table-column key="nuclear" prop="nuclear" :show-overflow-tooltip="true" label="核算单位" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.nuclear }}</span>
      </template>
    </el-table-column>
    <el-table-column key="totalMete" prop="totalMete" :show-overflow-tooltip="true" label="生产量" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.totalMete }}</span>
      </template>
    </el-table-column>
    <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice) : 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="总额（元）" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice) : 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column key="productionDate" prop="productionDate" :show-overflow-tooltip="true" label="生产日期" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.productionDate ? parseTime(scope.row.productionDate, '{y}/{m}/{d}') : '-' }}</span>
      </template>
    </el-table-column>
  </common-table>
  </div>
</template>

<script setup>
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'

const tableData = [
  {
    projectNumber: '11111',
    projectName: '项目',
    serialNumber: 'GL-3',
    specification: 'BH400*200*6*9',
    interfaceType: '>400<500',
    nuclear: '米',
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
    props: ['totalMete', ['totalPrice', DP.YUAN]]
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

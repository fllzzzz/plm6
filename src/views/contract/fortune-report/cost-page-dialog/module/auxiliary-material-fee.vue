<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table
          v-permission="permission.print"
          api-key="auxiliaryMaterialList"
          :params="{ basicClassEnum: mainAuxiliaryTypeEnum.AUXILIARY.V, projectId: props.costTypeData.projectId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
      <el-tag>合计（单位：元）：{{ toThousand(props.costTypeData?.amount,decimalPrecision.contract) }}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      :data="detailData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <!-- <el-table-column prop="basicClass" key="basicClass" label="物料种类" align="center" /> -->
      <el-table-column prop="classifyName" key="classifyName" label="物料种类" align="center" />
      <el-table-column prop="specification" key="specification" label="规格" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="accountingUnit" key="accountingUnit" label="核算单位" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" key="mete" label="核算量" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.mete) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.unitPrice }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="amount" key="amount" label="总价" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.amount,decimalPrecision.contract) || '月末未加权' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="outboundTime" key="outboundTime" label="出库日期" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.outboundTime ? parseTime(scope.row.outboundTime) : '-' }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </div>
</template>
<script setup>
import { getMainAuxiliaryList } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch } from 'vue'

import { matClsEnum } from '@enum-ms/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { mainAuxiliaryTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import usePagination from '@compos/use-pagination'

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchMainFee })

const props = defineProps({
  costTypeData: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { decimalPrecision } = useDecimalPrecision()

const tableRef = ref()
const detailData = ref([])

const { maxHeight } = useMaxHeight({
  paginate: true
})

const dataFormat = ref([
  ['basicClass', ['parse-enum', matClsEnum, { bit: true }]],
  ['avgPrice', ['to-thousand', decimalPrecision.contract]]
])

watch(
  () => props.costTypeData.projectId,
  (value) => {
    fetchMainFee()
  },
  { immediate: true, deep: true }
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['mete', ['amount', decimalPrecision.contract]],
    toThousandFields: ['mete', 'amount']
  })
}

async function fetchMainFee() {
  try {
    const { content = [], totalElements } = await getMainAuxiliaryList({
      basicClassEnum: mainAuxiliaryTypeEnum.AUXILIARY.V,
      projectId: props.costTypeData.projectId,
      ...queryPage
    })
    content.map((v) => {
      v.unitPrice = v.amount / v.mete
    })
    detailData.value = content || []
    detailData.value = await numFmtByBasicClass(
      detailData.value,
      {
        toSmallest: false,
        toNum: true
      },
      {
        mete: ['mete'],
        amount: ['unitPrice']
      }
    )
    setTotalPage(totalElements)
  } catch (error) {
    console.log('辅材费用', error)
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

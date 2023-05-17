<template>
  <div>
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table
          v-permission="permission.print"
          api-key="conMainMaterialList"
          :params="{ basicClassEnum: mainAuxiliaryTypeEnum.MAIN.V, projectId: props.costTypeData.projectId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
      <el-tag>合计（单位：元）：{{ toThousand(props.costTypeData?.amount) }}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      :data="detailData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight + 60"
      row-key="id"
      style="width: 100%"
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="basicClass" key="basicClass" label="物料种类" align="center" />
      <el-table-column prop="thickness" key="thickness" label="厚度" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.thickness }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specMerge" key="specMerge" label="规格" align="center" width="160">
        <template v-slot="scope">
          <span>{{ scope.row.specMerge }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column prop="specification" key="specification" label="材质" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column> -->
      <el-table-column prop="accountingUnit" key="accountingUnit" label="核算单位" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" key="mete" label="核算量" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.mete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.unitPrice?.toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="amount" key="amount" label="总价" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.amount ? toThousand(scope.row.amount) : 0 || '月末未加权' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="outboundTime" key="outboundTime" label="出库日期" align="center" width="140">
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
import { mainAuxiliaryTypeEnum } from '@enum-ms/contract'
// import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'

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

const tableRef = ref()
const detailData = ref([])

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

const dataFormat = ref([
  ['basicClass', ['parse-enum', matClsEnum, { bit: true }]],
  ['avgPrice', ['to-thousand-ck', 'YUAN']]
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
    props: ['mete', 'amount'],
    toThousandFields: ['mete', 'amount']
  })
}

async function fetchMainFee() {
  try {
    const { content = [], totalElements } = await getMainAuxiliaryList({
      basicClassEnum: mainAuxiliaryTypeEnum.MAIN.V,
      projectId: props.costTypeData.projectId,
      ...queryPage
    })
    setTotalPage(totalElements)
    content.forEach((v) => {
      v.unitPrice = v.mete ? v.amount / v.mete : 0
    })
    detailData.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    console.log(detailData.value, 'value')
    await setSpecInfoToList(detailData.value)
  } catch (error) {
    console.log('主材费用', error)
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

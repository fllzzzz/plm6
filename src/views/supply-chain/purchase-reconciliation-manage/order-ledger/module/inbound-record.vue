<template>
  <div class="app-container">
    <div v-show="!props.detailInfo?.id" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
    <div v-show="props.detailInfo?.id" style="width: 100%">
      <div style="margin-bottom:5px;">采购单号：{{detailInfo.serialNumber}} <el-tag>累计入库额：{{toThousand(detailInfo.inboundAmount)}}</el-tag></div>
      <div class="head-container">
        <el-input
          v-model="query.inboundSn"
          placeholder="入库单号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
        <common-button class="filter-item" size="small" type="warning" icon="el-icon-refresh" @click.stop="query.inboundSn=undefined;fetchList()">重置</common-button>
      </div>
      <common-table style="width: 100%;" :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <!-- 基础信息 -->
        <material-base-info-columns
          :columns="{}"
          spec-merge
        />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :columns="{}" />
        <!-- 价格信息 -->
        <amount-info-columns :columns="{}" :show-tax-rate="false"/>
        <el-table-column prop="inboundTime" label="入库时间" align="center" width="90" show-overflow-tooltip />
        <el-table-column prop="applicantName" label="入库人" align="center" show-overflow-tooltip width="90" />
        <el-table-column prop="reviewerName" label="审核人" align="center" show-overflow-tooltip width="90" />
        <el-table-column fixed="right" prop="inboundSerialNumber" label="入库单号" align="center" min-width="110" show-overflow-tooltip />
      </common-table>
      <!-- 分页 -->
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
  </div>
</template>
<script setup>
import { inboundRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineProps, watch } from 'vue'

import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { DP } from '@/settings/config'
import { tableSummary } from '@/utils/el-extra'
import { toThousand } from '@/utils/data-type/number'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const list = ref([])
const tableLoading = ref(false)
const query = ref({})
const dataFormat = ref([
  ['unitPrice', 'to-thousand'],
  ['amount', 'to-thousand'],
  ['unitPriceExcludingVAT', 'to-thousand'],
  ['amountExcludingVAT', 'to-thousand'],
  ['inputVAT', 'to-thousand'],
  ['inboundTime', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

watch(
  () => props.detailInfo.id,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

function getSummaries(param) {
  return tableSummary(param, {
    props: ['amount', 'amountExcludingVAT', 'inputVAT', ['mete', DP.COM_WT__KG]],
    toThousandFields: ['amount', 'amountExcludingVAT', 'inputVAT', 'mete']
  })
}

// 获取入库记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await inboundRecord({ orderId: props.detailInfo.id, ...queryPage, ...query.value })
    let hasContent = content
    if (hasContent.length > 0) {
      await setSpecInfoToList(hasContent)
      hasContent = await numFmtByBasicClass(hasContent)
    }
    _list = hasContent
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取入库记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
  width: 100%;
}
</style>

<template>
  <common-drawer
    ref="drawerRef"
    title="物流记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="90%"
  >
    <template #titleAfter>
      <el-tag type="warning" effect="plain" size="medium">物流公司：{{detailInfo.supplierName}}</el-tag>
      <el-tag>运费总额：{{detailInfo.totalPrice}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          api-key="productLogisticsRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
        <!-- <print-table
          v-permission="props.permission?.print"
          api-key="productLogisticsRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        /> -->
      </div>
    </template>
    <template #content>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="serialNumber" label="运输单号" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="auditTime" label="运输日期" align="center" width="100" show-overflow-tooltip />
        <el-table-column prop="userName" label="装车人" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="actualUserName" label="过磅复核人" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="actualWeight" label="过磅重量（吨）" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="carModel" label="车型" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="priceType" label="计价方式" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="totalPrice" label="总额" align="center" min-width="100" show-overflow-tooltip />
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
    </template>
  </common-drawer>
</template>

<script setup>
import { logisticsRecordDetail } from '@/api/supply-chain/logistics-payment-manage/jd-product-logistics-record-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { logisticsPriceTypeEnum } from '@enum-ms/mes'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  queryDate: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  // 汇总列表
  return {
    supplierId: props.detailInfo.supplierId,
    projectId: props.detailInfo.projectId
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { immediate: true }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['priceType', ['parse-enum', logisticsPriceTypeEnum]],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']],
  ['actualWeight', ['to-fixed', 2]],
  ['totalPrice', 'to-thousand']
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['totalPrice'],
    toThousandFields: ['totalPrice']
  })
}

// 获取物流记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await logisticsRecordDetail({ ...params.value, ...queryPage })
    content.map(v => {
      v.actualWeight = v.actualWeight ? v.actualWeight / 1000 : 0
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取物流记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

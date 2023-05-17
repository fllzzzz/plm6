<template>
  <common-drawer
    ref="drawerRef"
    title="入库记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="inbound-record"
    size="100%"
  >
    <template #titleAfter>
      <el-tag v-if="detailInfo.serialNumber" type="success" effect="plain" size="medium">采购合同编号：{{detailInfo.serialNumber}}</el-tag>
      <el-tag v-else type="warning" effect="plain" size="medium">供应商：{{detailInfo.supplierName}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="purchaseInboundRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <!-- 基础信息 -->
        <material-base-info-columns
          :columns="{}"
          spec-merge
        />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :columns="{}" />
        <!-- 价格信息 -->
        <amount-info-columns :columns="{}" :show-tax-rate="true"/>
        <el-table-column prop="inboundTime" label="入库时间" align="center" width="90" show-overflow-tooltip />
        <el-table-column prop="inboundSerialNumber" label="入库单号" align="center" min-width="110" show-overflow-tooltip />
        <el-table-column prop="applicantName" label="入库人" align="center" show-overflow-tooltip width="90" />
        <el-table-column prop="reviewerName" label="审核人" align="center" show-overflow-tooltip width="90" />
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
import { inboundRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { DP } from '@/settings/config'
import { tableSummary } from '@/utils/el-extra'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
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
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  // 订单列表
  if (props.detailInfo.id) {
    return {
      orderId: props.detailInfo.id
    }
  }
  // 汇总列表
  return {
    supplierId: props.detailInfo.supplierId
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['unitPrice', ['to-thousand-ck', 'YUAN']],
  ['amount', ['to-thousand-ck', 'YUAN']],
  ['unitPriceExcludingVAT', ['to-thousand-ck', 'YUAN']],
  ['amountExcludingVAT', ['to-thousand-ck', 'YUAN']],
  ['inputVAT', ['to-thousand-ck', 'YUAN']],
  ['inboundTime', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

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
    const { content = [], totalElements } = await inboundRecord({ ...params.value, ...queryPage })
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

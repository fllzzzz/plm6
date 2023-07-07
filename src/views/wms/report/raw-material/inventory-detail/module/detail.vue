<template>
  <common-drawer
    ref="drawerRef"
    title="存货明细账详情"
    v-model="drawerVisible"
    direction="rtl"
    :close-on-click-modal="false"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: props.rowDetail?.times?.[0], fmt: '{y}-{m}' }" /> ~
        <span v-parse-time="{ val: props.rowDetail?.times?.[1], fmt: '{y}-{m}' }" />
      </el-tag>
      <el-tag v-if="props.rowDetail?.project" type="success" effect="plain" size="medium">{{ props.rowDetail?.project?.name }}</el-tag>
      <el-tag type="warning" effect="plain" size="medium">核算单位：{{ props.rowDetail?.accountingUnit }}</el-tag>
    </template>
    <template #titleRight>
      <export-button
        v-permission="permission.download"
        :params="{
          times: props.rowDetail?.times,
          detailKey: props.rowDetail?.detailKey
        }"
        :fn="excel"
      >下载</export-button>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :data-format="dataFormat"
        :show-empty-symbol="false"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="createTime" prop="createTime" label="日期" align="center" width="100" />
        <el-table-column key="receipt" :show-overflow-tooltip="true" prop="receipt" label="单据编号" align="center" min-width="120">
          <template #default="{ row }">
            <receipt-sn-clickable v-if="row.receipt" :receipt-types="['INBOUND', 'OUTBOUND', 'TRANSFER', 'RETURN', 'REJECTED']" :receipt="row.receipt" />
          </template>
        </el-table-column>
        <el-table-column key="summary" prop="summary" label="摘要" :show-overflow-tooltip="true" min-width="120" align="center" />

        <el-table-column key="inboundData" prop="inboundData" label="入库数据" align="center">
          <el-table-column key="inboundMete" prop="inboundMete" align="center" min-width="120" label="数量" />
          <template v-if="showAmount">
            <el-table-column key="inboundUnitPriceExcludingVat" prop="inboundUnitPriceExcludingVat" align="right" min-width="120" label="单价(不含税)" />
            <el-table-column key="inboundAmountExcludingVat" prop="inboundAmountExcludingVat" align="right" min-width="120" label="金额(不含税)" />
          </template>
        </el-table-column>

        <el-table-column key="outboundData" prop="outboundData" label="出库数据" align="center">
          <el-table-column key="outboundMete" prop="outboundMete" align="center" min-width="120" label="数量" />
          <template v-if="showAmount">
            <el-table-column key="outboundUnitPriceExcludingVat" prop="outboundUnitPriceExcludingVat" align="right" min-width="120" label="单价(不含税)" />
            <el-table-column key="outboundAmountExcludingVat" prop="outboundAmountExcludingVat" align="right" min-width="120" label="金额(不含税)" />
          </template>
        </el-table-column>

        <el-table-column key="endData" prop="endData" label="结存数据" align="center">
          <el-table-column key="endMete" prop="endMete" align="center" min-width="120" label="数量" />
          <template v-if="showAmount">
            <el-table-column key="endUnitPriceExcludingVat" prop="endUnitPriceExcludingVat" align="right" min-width="120" label="单价(不含税)" />
            <el-table-column key="endAmountExcludingVat" prop="endAmountExcludingVat" align="right" min-width="120" label="金额(不含税)" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail, excel } from '@/api/wms/report/raw-material/inventory-detail'
import { defineProps, defineEmits, ref, watch, computed, inject } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import checkPermission from '@/utils/system/check-permission'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { DP } from '@/settings/config'

import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import ExportButton from '@comp-common/export-button/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  rowDetail: {
    type: Object,
    default: () => {}
  }
})

const permission = inject('permission')

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))

// 表格列数据格式转换
const dataFormat = computed(() => {
  return [
    ['createTime', ['parse-time', '{y}-{m}-{d}']],
    ['inboundUnitPriceExcludingVat', ['to-thousand', decimalPrecision.value.wms]],
    ['inboundAmountExcludingVat', ['to-thousand', DP.YUAN]],
    ['outboundUnitPriceExcludingVat', ['to-thousand', decimalPrecision.value.wms]],
    ['outboundAmountExcludingVat', ['to-thousand', DP.YUAN]],
    ['endUnitPriceExcludingVat', ['to-thousand', decimalPrecision.value.wms]],
    ['endAmountExcludingVat', ['to-thousand', DP.YUAN]]

  ]
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 49,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const tableLoading = ref(false)
const list = ref([])

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content = [] } = await detail({
      times: props.rowDetail.times,
      detailKey: props.rowDetail.detailKey
    })
    await setSpecInfoToList(content)
    _list = await numFmtByBasicClass(content, {
      ...props.rowDetail.measure,
      basicClass: props.rowDetail.basicClass
    }, {
      mete: ['inboundMete', 'outboundMete', 'endMete'],
      quantity: ['inboundQuantity', 'outboundQuantity', 'endQuantity'],
      amount: ['inboundUnitPriceExcludingVat', 'outboundUnitPriceExcludingVat', 'endUnitPriceExcludingVat']
    })
  } catch (error) {
    console.log('获取存货明细账详情列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

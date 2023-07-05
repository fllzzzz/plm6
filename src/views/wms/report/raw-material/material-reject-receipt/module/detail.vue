<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-inbound-application-record-detail"
  >
    <template #titleAfter>
      <title-after-info :order="order" :detail="detail" />
    </template>
    <template #titleRight>
      <purchase-detail-button v-permission="permission.purchaseOrderDetail" :purchase-id="order.id" size="mini" />
    </template>
    <template #content>
      <common-table
        :data="detail.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="detail.basicClass" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 价格信息 -->
        <template v-if="showAmount">
          <amount-info-columns v-if="!boolPartyA" show-unit-price-e/>
        </template>
        <warehouse-info-columns show-project show-monomer show-area />
      </common-table>
      <p class="remark">
        <span class="label-after">备注</span>
        <span v-empty-text="detail.remark" />
      </p>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, inject, ref } from 'vue'
import { orderSupplyTypeEnum } from '@enum-ms/wms'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import titleAfterInfo from '@/views/wms/material-reject/raw-material/components/title-after-info.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const permission = inject('permission')
const drawerRef = ref()
const expandRowKeys = ref([])
const { CRUD, crud, detail } = regDetail()

// 表格列数据格式转换
// const columnsDataFormat = ref([...materialHasAmountColumns])
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', decimalPrecision.value.wms]],
    ['amountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['inputVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['outboundReceipt.outboundTime', 'parse-time'],
    ['outboundReceipt.reviewTime', 'parse-time'],
    ['outboundReceipt.createTime', 'parse-time']
  ]
})

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-inbound-application-record-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 采购合同信息
const order = computed(() => detail.purchaseOrder || {})

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))
// 是否甲供订单
const boolPartyA = computed(() => order.value?.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  crud.detailLoading ? `退货单` : `退货单：${detail.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: false
  })
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'mete', ['amount', decimalPrecision.value.wms], ['amountExcludingVAT', decimalPrecision.value.wms], ['inputVAT', decimalPrecision.value.wms]],
    toThousandFields: ['amount', 'amountExcludingVAT', 'inputVAT']
  })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-record-detail {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
  .remark {
    font-size: 14px;
    :first-child {
      font-weight: 700;
    }
  }
}
</style>

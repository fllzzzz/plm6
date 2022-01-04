<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-inbound-application-review-detail"
  >
    <template #titleAfter>
      <title-after-info :order="order" :detail="detail" />
    </template>
    <template #titleRight>
      <purchase-detail-button v-if="showAmount" :purchase-id="order.id" size="mini" />
    </template>
    <template #content>
      <common-table
        :data="detail.list"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="detail.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info v-if="showAmount" :basic-class="detail.basicClass" :row="row" show-brand />
            <p>
              备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" fixed="left"/>
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns v-if="!showAmount" :basic-class="detail.basicClass" />
        <!-- 价格信息 -->
        <template v-if="showAmount">
          <amount-info-columns v-if="!boolPartyA" />
          <el-table-column prop="requisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
        </template>
        <warehouse-info-columns show-project />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { inboundFillWayEnum, orderSupplyTypeEnum } from '@enum-ms/wms'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import titleAfterInfo from '@/views/wms/inbound-components/title-after-info.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'

const drawerRef = ref()
const expandRowKeys = ref([])
const { CRUD, crud, detail } = regDetail()
const { inboundFillWayCfg } = useWmsConfig()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-inbound-application-record-preview',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 采购订单信息
const order = computed(() => detail.purchaseOrder || {})
// 显示金额
const showAmount = computed(() => inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.REVIEWING.V)
// 显示仓库
// const showWarehouse = computed(() => inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.REVIEWING.V)
// 是否甲供订单
const boolPartyA = computed(() => order.value.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  crud.detailLoading ? `入库单`
    : `入库单：${detail.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
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
  return tableSummary(param, { props: ['quantity', 'mete', 'amount', 'amountExcludingVAT', 'inputVAT'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-review-detail {
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }
  }
}
</style>

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
      <purchase-detail-button :purchase-id="order.id" size="mini" />
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
            <expand-secondary-info v-if="!showTableColumnSecondary" :basic-class="detail.basicClass" :row="row" show-brand />
            <p>
              备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" fixed="left" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns v-if="showTableColumnSecondary" :basic-class="detail.basicClass" />
        <!-- 价格信息 -->
        <template v-if="showAmount">
          <amount-info-columns v-if="!boolPartyA" />
          <el-table-column prop="requisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
          <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
            <template #default="{ row }">
              <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
            </template>
          </el-table-column>
        </template>
        <warehouse-info-columns v-if="showWarehouse" />
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
import titleAfterInfo from '@/views/wms/material-inbound/raw-material/components/title-after-info.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'

const drawerRef = ref()
const expandRowKeys = ref([])
const { CRUD, crud, detail } = regDetail()

const { inboundFillWayCfg } = useWmsConfig()

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

// 采购订单信息
const order = computed(() => detail.purchaseOrder || {})
// 显示金额
const showAmount = computed(() => inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V)
// 显示仓库
const showWarehouse = computed(() => inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.APPLICATION.V)
// 是否甲供订单
const boolPartyA = computed(() => order.value.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  crud.detailLoading ? `入库单` : `入库单：${detail.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

// 在列中显示次要信息
const showTableColumnSecondary = computed(() => {
  // 非甲供订单，显示项目和申购单 或者仓库时
  const unshow1 = showAmount.value && !boolPartyA.value && ((order.value.projects && order.value.requisitionsSN) || showWarehouse.value)
  // 甲供订单，显示项目和申购单以及仓库时
  const unshow2 = showAmount.value && boolPartyA.value && order.value.projects && order.value.requisitionsSN && showWarehouse.value
  return !(unshow1 || unshow2)
})

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
    props: ['quantity', 'mete', 'amount', 'amountExcludingVAT', 'inputVAT'],
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
}
</style>

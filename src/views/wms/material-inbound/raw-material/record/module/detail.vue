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
        :data-format="columnsDataFormat"
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
              单体：<span>{{ row.monomerName }}</span>
            </p>
            <p>
              区域：<span>{{ row.areaName }}</span>
            </p>
            <p>
              备注：<span>{{ row.remark }}</span>
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
        </template>
        <el-table-column prop="requisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
        <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip />
        <el-table-column prop="monomerName" label="单体" align="left" min-width="120px" show-overflow-tooltip />
        <el-table-column prop="areaName" label="区域" align="left" min-width="120px" show-overflow-tooltip />
        <warehouse-info-columns />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { inject, computed, ref } from 'vue'
import { orderSupplyTypeEnum, inboundFillWayEnum } from '@enum-ms/wms'
import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

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
import checkPermission from '@/utils/system/check-permission'

const permission = inject('permission')
// 表格列数据格式转换
const columnsDataFormat = ref([...materialHasAmountColumns, ['remark', 'empty-text'], ['monomerName', 'empty-text'], ['areaName', 'empty-text']])

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

// 采购合同信息
const order = computed(() => detail.purchaseOrder || {})

// 显示金额相关信息（（统一为入库填写，取消后台配置）
// const fillableAmount = ref(false)
const fillableAmount = computed(() =>
  inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V : false
)

// 显示金额
const showAmount = computed(() => checkPermission(permission.showAmount) || fillableAmount.value)
// 是否甲供订单
const boolPartyA = computed(() => order.value?.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  crud.detailLoading ? `入库单` : `入库单：${detail.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : '无供应商'} ）`
)

// 在列中显示次要信息
const showTableColumnSecondary = computed(() => {
  // 非甲供订单，显示项目和申购单 或者仓库时
  const unshow1 = showAmount.value && !boolPartyA.value && order.value.projects && order.value.requisitionsSN
  // 甲供订单，显示项目和申购单以及仓库时
  const unshow2 = showAmount.value && boolPartyA.value && order.value.projects && order.value.requisitionsSN
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
    props: ['quantity', 'mete', ['amount', DP.YUAN], ['amountExcludingVAT', DP.YUAN], ['inputVAT', DP.YUAN]],
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

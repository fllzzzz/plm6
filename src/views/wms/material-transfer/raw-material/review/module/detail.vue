<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-transfer-application-review-detail"
  >
    <template #titleAfter>
      <common-title-info :detail="detail" />
    </template>
    <template #titleRight>
      <span class="invoice-type-info" v-if="showAmount && isNotBlank(detail.invoiceType)">
        <span v-parse-enum="{ e: invoiceTypeEnum, v: detail.invoiceType }" />
        <span v-if="detail.invoiceType !== invoiceTypeEnum.RECEIPT.V">（{{ detail.taxRate }}%）</span>
      </span>
      <el-tag
        v-if="isNotBlank(detail.reviewStatus)"
        :type="reviewStatusEnum.V[detail.reviewStatus].TAG"
        size="medium"
        style="margin-right: 10px"
      >
        {{ reviewStatusEnum.VL[detail.reviewStatus] }}
      </el-tag>
    </template>
    <template #content>
      <unfreeze-info class="unfreeze-info" v-if="detail.boolHasUnfreeze" :basic-class="detail.basicClass" :list="detail.unfreezeList" />
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
            <expand-secondary-info v-if="showAmount" :basic-class="detail.basicClass" :row="row" show-brand>
              <p>
                备注：<span v-empty-text>{{ row.remark }}</span>
              </p>
            </expand-secondary-info>
            <p v-else>
              备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" show-party-a-transfer fixed="left" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns v-if="!showAmount" :basic-class="detail.basicClass" />
        <!-- 价格信息 -->
        <template v-if="showAmount">
          <amount-info-columns />
        </template>
        <warehouse-info-columns show-project />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { partyAMatTransferEnum, transferTypeEnum } from '@enum-ms/wms'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { isNotBlank } from '@/utils/data-type'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import commonTitleInfo from './common-title-info.vue'
import unfreezeInfo from './unfreeze-info.vue'

const drawerRef = ref()
const expandRowKeys = ref([]) // 展开key
const showAmount = ref(false) // 显示金额，只有“买入甲供材料才需要填写金额”
const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-transfer-application-review-detail',
    extraBox: ['.el-drawer__header', '.unfreeze-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 标题
const drawerTitle = computed(() => (crud.detailLoading ? `调拨单：` : `调拨单：${detail.serialNumber || ''}`))

// 详情加载之前
CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: false
  })
  // 将甲供材料调拨到其他项目或公共库中时，需要填写金额
  if (detail.transferType !== transferTypeEnum.RETURN_PARTY_A.V) {
    let partyANum = 0
    // 遍历判断是否存在甲供材料, 并且甲供材料属于“买入”类型
    detail.list.forEach((v) => {
      if (v.boolPartyA && v.partyATransferType === partyAMatTransferEnum.BUY_IN.V) partyANum++
    })
    showAmount.value = partyANum > 0
  } else {
    showAmount.value = false
  }
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
.raw-mat-transfer-application-review-detail {
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }
  }

  .invoice-type-info {
    font-weight: bold;
    color: brown;
  }
}
</style>

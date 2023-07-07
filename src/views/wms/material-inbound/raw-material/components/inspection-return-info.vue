<template>
  <span v-bind="$attrs" class="tip" @click="openView">
    * 当前入库单据存在质检退回物料，点击可查看质检退回的物料信息
  </span>
  <common-dialog
    title="质检退回物料列表"
    v-model="dialogVisible"
    width="95%"
    :show-close="true"
    custom-class="wms-inspection-return-list"
    top="10vh"
  >
    <common-table
      ref="tableRef"
      :data="list"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="!showTableColumnSecondary" :basic-class="basicClass" :row="row" show-brand />
          <p>
            备注：<span>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :basic-class="basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :basic-class="basicClass" />
      <!-- 次要信息 -->
      <material-secondary-info-columns v-if="showTableColumnSecondary" :basic-class="basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns v-if="!boolPartyA" />
      </template>
      <el-table-column prop="requisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
      <warehouse-info-columns show-project />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed } from 'vue'
import { materialColumns } from '@/utils/columns-format/wms'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

defineProps({
  basicClass: {
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  },
  showTableColumnSecondary: {
    type: Boolean
  },
  showAmount: {
    type: Boolean
  },
  boolPartyA: {
    type: Boolean
  }
})

const dialogVisible = ref()
const expandRowKeys = ref()
// 表格列数据格式转换
// const columnsDataFormat = ref([...materialHasAmountColumns, ['remark', 'empty-text']])
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', DP.YUAN]],
    ['amountExcludingVAT', ['to-thousand', DP.YUAN]],
    ['inputVAT', ['to-thousand', DP.YUAN]],
    ['remark', 'empty-text']
  ]
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-inspection-return-list',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

// 查看信息
function openView() {
  dialogVisible.value = true
}
</script>

<style lang="scss" scoped>
.tip {
  display: inline-block;
  cursor: pointer;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}

.el-table {
  ::v-deep(td .cell) {
    min-height: 28px;
    line-height: 28px;
  }
}
</style>

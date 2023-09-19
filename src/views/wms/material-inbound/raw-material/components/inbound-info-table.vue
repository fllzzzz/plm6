<template>
  <common-table
    :data="list"
    :data-format="columnsDataFormat"
    :show-summary="false"
    :summary-method="getSummaries"
    row-key="id"
    style="width: 100%"
    :class="{ 'table-border-none': !hasBorder }"
  >
    <!-- 基础信息 -->
    <material-base-info-columns :basic-class="basicClass">
      <template #afterIndex>
        <el-table-column prop="inboundTime" show-overflow-tooltip align="center" width="100" label="入库时间" />
        <el-table-column prop="applyPurchaseSN" show-overflow-tooltip align="center" min-width="120" label="申购单" />
      </template>
    </material-base-info-columns>
    <!-- 单位及其数量 -->
    <material-unit-quantity-columns :basic-class="basicClass" />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="basicClass" />
    <el-table-column prop="remark" show-overflow-tooltip align="left" min-width="120" label="备注" />
    <!-- <warehouse-info-columns show-project show-monomer show-area /> -->
  </common-table>
</template>

<script setup>
import { tableSummary } from '@/utils/el-extra'
import { defineProps, defineEmits, computed, ref } from 'vue'
import { materialStatusEnum } from '@/views/wms/material-reject/enum'
import { isBlank } from '@/utils/data-type'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { materialNestedColumns } from '@/utils/columns-format/wms'

import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
// import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

const emit = defineEmits(['del'])
const props = defineProps({
  hasBorder: {
    type: Boolean,
    default: false,
  },
  material: {
    type: Object,
    default: () => {
      return {}
    },
  },
  basicClass: {
    type: Number,
  },
  list: {
    type: Array,
    default: () => [],
  },
})

// 表格列数据格式转换
const columnsDataFormat = ref([...materialNestedColumns, ['inboundTime', ['parse-time', '{y}-{m}-{d}']]])

// 合计
function getSummaries(param) {
  // 获取单位精度
  const mDP = props.material.measurePrecision || 0
  const aDP = props.material.accountingPrecision || 2
  return tableSummary(param, {
    props: [
      ['quantity', mDP],
      ['mete', aDP],
    ],
  })
}
</script>

<style lang="scss" scoped>
.table-border-none {
  max-width: 90%;
  ::v-deep(.cell) {
    height: 30px;
    line-height: 30px;
  }
  ::v-deep(th.el-table__cell) {
    // background-color: #3a8ee6;
    background-color: #65bdcf;
    color: white;
  }
  ::v-deep(td.el-table__cell) {
    background-color: #ecf5ff;
  }
  ::v-deep(.el-table__footer-wrapper) {
    td.el-table__cell {
      background-color: #0000001f;
    }
  }
}
</style>

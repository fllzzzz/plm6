<template>
  <common-table
    :data="list"
    show-summary
    :summary-method="getSummaries"
    row-key="id"
    class="raw-mat-reject-info-table"
    :class="{ 'table-border-none': !hasBorder }"
  >
    <!-- 基础信息 -->
    <material-base-info-columns :basic-class="basicClass" spec-merge />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="basicClass" />
    <!-- 单位及其数量 -->
    <material-unit-quantity-columns :basic-class="basicClass" outbound-type-mode :number-prop-field="numberPropField" />
    <warehouse-info-columns show-project />
    <el-table-column key="status" prop="status" label="状态" align="center" width="80" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tag v-parse-enum="{ e: materialStatusEnum, v: row.reviewStatus }" :type="materialStatusEnum.V[row.reviewStatus].TAG" />
      </template>
    </el-table-column>
    <el-table-column key="createTime" prop="createTime" label="退货日期" align="center" width="120" show-overflow-tooltip>
      <template #default="{ row, $index }">
        <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
        <common-button
          v-if="row.reviewStatus === materialStatusEnum.UNSUBMITTED.V && operable"
          type="danger"
          size="mini"
          icon="el-icon-delete"
          @click="delRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { tableSummary } from '@/utils/el-extra'
import { defineProps, defineEmits, computed } from 'vue'
import { materialStatusEnum } from '@/views/wms/material-reject/enum'

import materialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-custom-field-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-custom-field-columns/warehouse-info-columns/index.vue'
import { isBlank } from '@/utils/data-type'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

const emit = defineEmits(['del'])
const props = defineProps({
  operable: {
    type: Boolean,
    default: false
  },
  hasBorder: {
    type: Boolean,
    default: false
  },
  material: {
    type: Object,
    default: () => {
      return {}
    }
  },
  basicClass: {
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  }
})

// 出库单位对应的字段
const numberPropField = computed(() => {
  if (isBlank(props.material)) return
  if (props.material.outboundUnitType === measureTypeEnum.MEASURE.V) {
    return 'quantity'
  } else {
    return 'mete'
  }
})

// 合计
function getSummaries(param) {
  // 获取单位精度
  const mDP = props.material.measurePrecision || 0
  const aDP = props.material.accountingPrecision || 2
  return tableSummary(param, {
    props: [
      ['material.quantity', mDP],
      ['material.mete', aDP]
    ]
  })
}

// 删除行
function delRow(row, index) {
  const list = props.list
  list.splice(index, 1)
  emit('del', props.material, row)
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

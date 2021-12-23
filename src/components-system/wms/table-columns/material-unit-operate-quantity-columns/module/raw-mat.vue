<template>
  <el-table-column v-if="showMeasureUnit" prop="measureUnit" label="计量单位" align="center" width="70px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text>{{ row.measureUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showQuantity"
    prop="quantity"
    :label="quantityLabel"
    show-overflow-tooltip
    align="right"
    :min-width="showOperableQuantity ? '150px' : '70px'"
  >
    <template #default="{ row }">
      <template v-if="row.measureUnit">
        <template v-if="showOperableQuantity">
          <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableQuantityField], dp: row.measurePrecision }" />
          /
        </template>
        <span v-empty-text v-to-fixed="row.measurePrecision">{{ row[quantityField] }}</span>
      </template>
      <span v-else v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showAccountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text>{{ row.accountingUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showMete" prop="mete" :label="meteLabel" align="right" min-width="150px" show-overflow-tooltip>
    <template #default="{ row }">
      <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableMeteField], dp: row.accountingPrecision }" />
      /
      <span v-empty-text v-to-fixed="row.accountingPrecision">{{ row[meteField] }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  showUnit: {
    // 是否显示单位
    type: Boolean,
    default: true
  },
  showSteelUnit: {
    // 是否显示钢材单位
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  },
  singleMeteMode: {
    // 单量模式,只显示单件核算量
    type: Boolean,
    default: false
  },
  meteLabel: {
    // 量-label
    type: String
  },
  quantityField: {
    // 数量字段
    type: String,
    default: 'quantity'
  },
  meteField: {
    // 核算量量字段
    type: String,
    default: 'mete'
  },
  operableQuantityField: {
    // 可操作数量字段
    type: String,
    default: 'operableQuantity'
  },
  operableMeteField: {
    // 可操作核算量量字段
    type: String,
    default: 'operableMete'
  },
  showOperableQuantity: {
    // 显示可操作数量
    type: Boolean,
    default: true
  }
})

const meteLabel = computed(() => {
  if (props.meteLabel) {
    return props.meteLabel
  }
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
      return props.singleMeteMode ? '单重（kg）' : '重量（kg）'
    case rawMatClsEnum.STEEL_COIL.V:
      return '重量（kg）'
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return props.singleMeteMode ? '单件量（kg）' : '核算量'
  }
})

const quantityLabel = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return '数量（张）'
    case rawMatClsEnum.SECTION_STEEL.V:
      return '数量（根）'
    case rawMatClsEnum.STEEL_COIL.V:
      return '长度（mm）'
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return '数量'
  }
})

// 是否显示单位
const showUnit = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return props.showSteelUnit && props.showUnit
  } else {
    return props.showUnit
  }
})

const showMeasureUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible('measureUnit')))
const showAccountingUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible('accountingUnit')))
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible('quantity'))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible('mete'))
</script>

<style lang="scss" scoped>
.operable-number {
  color: green;
}
</style>

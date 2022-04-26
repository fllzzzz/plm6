<template>
  <!-- 退货 -->
  <template v-if="outboundTypeMode">
    <el-table-column
      v-if="showOutboundUnit"
      key="rejectUnit"
      prop="rejectUnit"
      label="单位"
      align="center"
      width="70px"
      :fixed="fixed"
      show-overflow-tooltip
    />
    <el-table-column
      v-if="showNumber"
      :key="numberPropField"
      :prop="numberPropField"
      label="可操作数量/数量"
      align="right"
      :fixed="fixed"
      show-overflow-tooltip
      :min-width="!equalDisabled ? '150px' : '70px'"
    >
      <template #default="{ row }">
        <!-- 计量 -->
        <template v-if="row.outboundUnitType === measureTypeEnum.MEASURE.V">
          <template v-if="!equalDisabled || row[operableQuantityField] != row[quantityField]">
            <span class="color-green">{{ row[operableQuantityField] }}</span>
            /
          </template>
        </template>
        <!-- 计量 -->
        <template v-if="row.outboundUnitType === measureTypeEnum.ACCOUNTING.V">
          <template v-if="!equalDisabled || row[operableMeteField] != row[meteField]">
            <span class="color-green">{{ row[operableMeteField] }}</span>
            /
          </template>
        </template>
        <span>{{ row.outboundUnitType === measureTypeEnum.MEASURE.V ? row[quantityField] : row[meteField] }}</span>
      </template>
    </el-table-column>
  </template>
  <template v-else-if="rejectTypeMode">
    <el-table-column
      v-if="showRejectUnit"
      key="rejectUnit"
      prop="rejectUnit"
      label="单位"
      align="center"
      width="70px"
      :fixed="fixed"
      show-overflow-tooltip
    />
    <el-table-column
      v-if="showNumber"
      :key="numberPropField"
      :prop="numberPropField"
      label="可操作数量/数量"
      align="right"
      :fixed="fixed"
      show-overflow-tooltip
      :min-width="!equalDisabled ? '150px' : '70px'"
    >
      <template #default="{ row }">
        <!-- 计量 -->
        <template v-if="row.rejectUnitType === measureTypeEnum.MEASURE.V">
          <template v-if="!equalDisabled || row[operableQuantityField] != row[quantityField]">
            <span class="color-green">{{ row[operableQuantityField] }}</span>
            /
          </template>
        </template>
        <!-- 计量 -->
        <template v-if="row.rejectUnitType === measureTypeEnum.ACCOUNTING.V">
          <template v-if="!equalDisabled || row[operableMeteField] != row[meteField]">
            <span class="color-green">{{ row[operableMeteField] }}</span>
            /
          </template>
        </template>
        <span>{{ row.rejectUnitType === measureTypeEnum.MEASURE.V ? row[quantityField] : row[meteField] }}</span>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column
      v-if="showMeasureUnit"
      prop="measureUnit"
      key="measureUnit"
      label="计量单位"
      align="center"
      width="70px"
      show-overflow-tooltip
    />
    <el-table-column
      v-if="showQuantity"
      key="quantity"
      prop="quantity"
      :label="quantityLabel"
      show-overflow-tooltip
      align="right"
      :min-width="showOperableQuantity ? '150px' : '70px'"
    >
      <template #default="{ row }">
        <template v-if="row.sourceRow.measureUnit">
          <!-- 显示操作数量，并且可操作数量 != 实际数量-->
          <template v-if="showOperableQuantity && (!equalDisabled || row[operableQuantityField] != row[quantityField])">
            <span class="color-green">{{ row[operableQuantityField] }}</span>
            /
          </template>
          {{ row[quantityField] }}
        </template>
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showAccountingUnit"
      key="accountingUnit"
      prop="accountingUnit"
      label="核算单位"
      align="center"
      width="70px"
      show-overflow-tooltip
    />
    <el-table-column v-if="showMete" key="mete" prop="mete" :label="meteLabel" align="right" min-width="150px" show-overflow-tooltip>
      <template #default="{ row }">
        <template v-if="showOperableMete && (!equalDisabled || row[operableMeteField] != row[meteField])">
          <span class="color-green">{{ row[operableMeteField] }}</span>
          /
        </template>
        {{ row[meteField] }}
      </template>
    </el-table-column>
  </template>
</template>
<script setup>
import { defineProps, computed } from 'vue'
import { MAT_BASE_UNIT, STEEL_ENUM } from '@/settings/config'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
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
  showMeasureUnit: {
    // 是否显示计量单位
    type: Boolean,
    default: true
  },
  showQuantity: {
    // 显示数量
    type: Boolean,
    default: true
  },
  showSteelUnit: {
    // 是否显示钢材单位
    type: Boolean,
    default: false
  },
  fixed: {
    // 固定列
    type: String
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
  labelPrefix: {
    // 列名前缀
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
  },
  showOperableMete: {
    type: Boolean,
    default: true
  },
  equalDisabled: {
    type: Boolean,
    default: false
  },
  rejectTypeMode: {
    type: Boolean,
    default: false
  },
  // 出库模式
  outboundTypeMode: {
    type: Boolean,
    default: false
  },
  showNumber: {
    // 是否显示数量
    type: Boolean,
    default: true
  },
  numberPropField: {
    // 默认计量，用于合计时
    type: String,
    default: 'quantity'
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit()

const unitInfo = computed(() => {
  if (props.basicClass) {
    if (loaded.value) {
      return baseUnit.value[props.basicClass] || {}
    } else {
      return MAT_BASE_UNIT[props.basicClass] || {}
    }
  } else {
    return {}
  }
})

const meteLabel = computed(() => {
  let label = ''
  if (props.meteLabel) {
    label = props.meteLabel
  }
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
      label = props.singleMeteMode ? `单重(${unitInfo.value.weight.unit})` : `重量(${unitInfo.value.weight.unit})`
      break
    case rawMatClsEnum.STEEL_COIL.V:
      label = `重量(${unitInfo.value.weight.unit})`
      break
    case STEEL_ENUM:
      label = props.singleMeteMode ? `单重(${unitInfo.value.weight.unit})` : `重量(${unitInfo.value.weight.unit})`
      break
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      label = props.singleMeteMode ? `单件量` : '核算量'
      break
  }
  if (props.showOperableMete && isNotBlank(props.labelPrefix)) {
    label = props.labelPrefix + ' / ' + label
  }
  return label
})

const quantityLabel = computed(() => {
  let label = ''
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      label = `数量(${unitInfo.value.measure.unit})`
      break
    case rawMatClsEnum.SECTION_STEEL.V:
      label = `数量(${unitInfo.value.measure.unit})`
      break
    case rawMatClsEnum.STEEL_COIL.V:
      label = `长度(${unitInfo.value.measure.unit})`
      break
    case STEEL_ENUM:
      label = `数量`
      break
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      label = '数量'
      break
  }
  if (props.showOperableQuantity && isNotBlank(props.labelPrefix)) {
    label = props.labelPrefix + ' / ' + label
  }
  return label
})

// 是否显示单位
const showUnit = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return props.showSteelUnit && props.showUnit
  } else {
    return props.showUnit
  }
})

const showMeasureUnit = computed(
  () => props.showMeasureUnit && showUnit.value && (isBlank(props.columns) || props.columns.visible('measureUnit'))
)
const showAccountingUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible('accountingUnit')))
const showQuantity = computed(() => props.showQuantity && (isBlank(props.columns) || props.columns.visible('quantity')))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible('mete'))

const showOutboundUnit = computed(() => isBlank(props.columns) || props.columns.visible('outboundUnit'))
const showRejectUnit = computed(() => isBlank(props.columns) || props.columns.visible('rejectUnit'))
const showNumber = computed(() => props.showNumber && (isBlank(props.columns) || props.columns.visible(props.numberPropField)))
</script>

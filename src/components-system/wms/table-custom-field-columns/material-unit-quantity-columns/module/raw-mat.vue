<template>
  <template v-if="loaded">
    <template v-if="outboundTypeMode">
      <el-table-column
        v-if="showOutboundUnit"
        :key="`${field}.outboundUnit`"
        :prop="`${field}.outboundUnit`"
        label="单位"
        align="center"
        width="70px"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ getInfo(row, 'outboundUnit') }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="showCurQuantity"
        :key="`${field}.curQuantity`"
        :prop="`${field}.curQuantity`"
        label="数量"
        align="right"
        width="100px"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text v-to-fixed="getInfo(row, 'outboundUnitPrecision')">
            {{ getInfo(row, 'curOutboundUnitType') === measureTypeEnum.MEASURE.V ? getInfo(row, quantityField) : getInfo(row, meteField) }}
          </span>
        </template>
      </el-table-column>
    </template>
    <template v-else>
      <el-table-column
        v-if="showMeasureUnit"
        :key="`${field}.measureUnit`"
        :prop="`${field}.measureUnit`"
        label="计量单位"
        align="center"
        width="70px"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ getInfo(row, 'measureUnit') }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="showQuantity"
        :key="`${field}.${quantityField}`"
        :prop="`${field}.${quantityField}`"
        :label="quantityLabel"
        align="right"
        width="100px"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-if="getInfo(row, 'measureUnit')" v-empty-text v-to-fixed="getInfo(row, 'measurePrecision')">
            {{ getInfo(row, quantityField) }}
          </span>
          <span v-else v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="showAccountingUnit"
        :key="`${field}.accountingUnit`"
        :prop="`${field}.accountingUnit`"
        label="核算单位"
        align="center"
        width="70px"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ getInfo(row, 'accountingUnit') }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="showMete" :prop="`${field}.${meteField}`" :label="mateLabel" align="right" width="100px" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text v-to-fixed="getInfo(row, 'accountingPrecision')">{{ getInfo(row, meteField) }}</span>
        </template>
      </el-table-column>
    </template>
  </template>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { STEEL_ENUM } from '@/settings/config'
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
  showSteelUnit: {
    // 是否显示钢材单位
    type: Boolean,
    default: false
  },
  outboundTypeMode: {
    // 出库单位 模式(显示出库单位对应的数量及单位)
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  },
  labelPrefix: {
    // 数量label前缀
    type: String
  },
  quantityField: {
    // 数量字段
    type: String,
    default: 'quantity'
  },
  meteField: {
    // 核算量字段
    type: String,
    default: 'mete'
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const getInfo = inject('getInfo')

const { loaded, baseUnit } = useMatBaseUnit()

const unitInfo = computed(() => {
  if (props.basicClass) {
    return baseUnit.value[props.basicClass] || {}
  } else {
    return {}
  }
})

const mateLabel = computed(() => {
  let label = ''
  if (props.showUnit && props.showSteelUnit) {
    label = '核算量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
      case rawMatClsEnum.SECTION_STEEL.V:
      case rawMatClsEnum.STEEL_COIL.V:
      case STEEL_ENUM:
        label = `重量(${unitInfo.value.weight.unit})`
        break
      case rawMatClsEnum.MATERIAL.V:
      case rawMatClsEnum.GAS.V:
      default:
        label = '核算量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

const quantityLabel = computed(() => {
  let label = ''
  if (props.showUnit && props.showSteelUnit) {
    label = '数量'
  } else {
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
        label = `数量(${unitInfo.value.measure.unit})`
        break
      case rawMatClsEnum.MATERIAL.V:
      case rawMatClsEnum.GAS.V:
      default:
        label = '数量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

// 是否显示单位
const showUnit = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return props.showSteelUnit && props.showUnit
  } else {
    return props.showUnit
  }
})

const showMeasureUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible(`${props.field}.measureUnit`)))
const showAccountingUnit = computed(
  () => showUnit.value && (isBlank(props.columns) || props.columns.visible(`${props.field}.accountingUnit`))
)
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.${props.quantityField}`))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.${props.meteField}`))

const showOutboundUnit = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.outboundUnit`))
const showCurQuantity = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.curQuantity`))
</script>

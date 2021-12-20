<template>
  <template v-if="outboundTypeMode">
    <el-table-column v-if="showOutboundUnit" :prop="`${field}.outboundUnit`" label="单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'outboundUnit') }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showCurQuantity" :prop="`${field}.curQuantity`" label="数量" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="getInfo(row, 'outboundUnitPrecision')">
          {{ getInfo(row, 'curOutboundUnitType') === measureTypeEnum.MEASURE.V ? getInfo(row, quantityField) : getInfo(row, meteField) }}
        </span>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column v-if="showMeasureUnit" :prop="`${field}.measureUnit`" label="计量单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'measureUnit') }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showQuantity" :prop="`${field}.${quantityField}`" :label="quantityLabel" align="right" width="100px">
      <template #default="{ row }">
        <span v-if="getInfo(row, 'measureUnit')" v-empty-text v-to-fixed="getInfo(row, 'measurePrecision')">
          {{ getInfo(row, quantityField) }}
        </span>
        <span v-else v-empty-text />
      </template>
    </el-table-column>
    <el-table-column v-if="showAccountingUnit" :prop="`${field}.accountingUnit`" label="核算单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'accountingUnit') }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showMete" :prop="`${field}.${meteField}`" :label="mateLabel" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="getInfo(row, 'accountingPrecision')">{{ getInfo(row, meteField) }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

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
  outboundTypeMode: {
    // 出库单位 模式（显示出库单位对应的数量及单位）
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

const mateLabel = computed(() => {
  let label = ''
  if (props.showUnit) {
    label = '核算量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
      case rawMatClsEnum.SECTION_STEEL.V:
      case rawMatClsEnum.STEEL_COIL.V:
        label = '重量(kg)'
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
  if (props.showUnit) {
    label = '数量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
        label = '数量(张)'
        break
      case rawMatClsEnum.SECTION_STEEL.V:
        label = '数量(根)'
        break
      case rawMatClsEnum.STEEL_COIL.V:
        label = '长度(mm)'
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

const showMeasureUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible(`${props.field}.measureUnit`)))
const showAccountingUnit = computed(
  () => props.showUnit && (isBlank(props.columns) || props.columns.visible(`${props.field}.accountingUnit`))
)
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.${props.quantityField}`))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.${props.meteField}`))

const showOutboundUnit = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.outboundUnit`))
const showCurQuantity = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.curQuantity`))
</script>

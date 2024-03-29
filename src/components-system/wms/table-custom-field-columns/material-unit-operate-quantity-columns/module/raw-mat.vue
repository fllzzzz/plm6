<template>
  <template v-if="loaded">
    <el-table-column
      v-if="showMeasureUnit"
      :key="`${field}.measureUnit`"
      :prop="`${field}.measureUnit`"
      label="计量单位"
      align="center"
      width="70px"
      show-overflow-tooltip
    />
    <el-table-column
      v-if="showQuantity"
      :key="`${field}.quantity`"
      :prop="`${field}.quantity`"
      :label="quantityLabel"
      align="right"
      min-width="150px"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <template v-if="getInfo(row.sourceRow, 'measureUnit')">
          <span class="color-green">{{ getInfo(row, 'operableQuantity') }}</span>
          /
          {{ getInfo(row, 'quantity') }}
        </template>
        <span v-else>-</span>
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
    />
    <el-table-column
      v-if="showMete"
      :key="`${field}.mete`"
      :prop="`${field}.mete`"
      :label="mateLabel"
      align="right"
      min-width="150px"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <span class="color-green">{{ getInfo(row, 'operableMete') }}</span>
        /
        {{ getInfo(row, 'mete') }}
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { STEEL_ENUM } from '@/settings/config'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

const props = defineProps({
  basicClass: {
    type: Number
  },
  showUnit: {
    type: Boolean,
    default: true
  },
  columns: {
    type: Object
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
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V:
    case STEEL_ENUM:
      return `重量(${unitInfo.value.weight.unit})`
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return '核算量'
  }
})

const quantityLabel = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return `数量(${unitInfo.value.measure.unit})`
    case rawMatClsEnum.SECTION_STEEL.V:
      return `数量(${unitInfo.value.measure.unit})`
    case rawMatClsEnum.STEEL_COIL.V:
      return `长度(${unitInfo.value.measure.unit})`
    case STEEL_ENUM:
      return `数量`
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return '数量'
  }
})

const showMeasureUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible(`${props.field}.measureUnit`)))
const showAccountingUnit = computed(
  () => props.showUnit && (isBlank(props.columns) || props.columns.visible(`${props.field}.accountingUnit`))
)
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.quantity`))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.mete`))
</script>

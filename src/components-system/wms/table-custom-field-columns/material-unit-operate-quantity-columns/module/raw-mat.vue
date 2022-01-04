<template>
  <template v-if="loaded">
    <el-table-column
      v-if="showMeasureUnit"
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
      :prop="`${field}.quantity`"
      :label="quantityLabel"
      align="right"
      min-width="150px"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <template v-if="getInfo(row, 'measureUnit')">
          <span class="operable-number" v-empty-text v-to-fixed="getInfo(row, 'measurePrecision')">
            {{ getInfo(row, 'operableQuantity') }}
          </span>
          /
          <span v-empty-text v-to-fixed="getInfo(row, 'measurePrecision')">{{ getInfo(row, 'quantity') }}</span>
        </template>
        <span v-else v-empty-text />
      </template>
    </el-table-column>
    <el-table-column
      v-if="showAccountingUnit"
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
    <el-table-column v-if="showMete" :prop="`${field}.mete`" :label="mateLabel" align="right" min-width="150px" show-overflow-tooltip>
      <template #default="{ row }">
        <span class="operable-number" v-empty-text v-to-fixed="getInfo(row, 'accountingPrecision')">
          {{ getInfo(row, 'operableMete') }}
        </span>
        /
        <span v-empty-text v-to-fixed="getInfo(row, 'accountingPrecision')">{{ getInfo(row, 'mete') }}</span>
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
      return `数量(${unitInfo.value.measure.unit})`
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

<style lang="scss" scoped>
.operable-number {
  color: green;
}
</style>

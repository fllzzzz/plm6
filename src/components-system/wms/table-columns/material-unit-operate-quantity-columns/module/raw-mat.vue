<template>
  <el-table-column v-if="showMeasureUnit" prop="measureUnit" label="计量单位" align="center" width="70px">
    <template #default="{ row }">
      <span v-empty-text>{{ row.measureUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showQuantity" prop="quantity" :label="quantityLabel" align="right" min-width="150px">
    <template #default="{ row }">
      <template v-if="row.measureUnit">
        <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableQuantityField], dp: row.measurePrecision }" />
        /
        <span v-empty-text v-to-fixed="row.measurePrecision">{{ row.quantity }}</span>
      </template>
      <span v-else v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showAccountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px">
    <template #default="{ row }">
      <span v-empty-text>{{ row.accountingUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showMete" prop="mete" :label="mateLabel" align="right" min-width="150px">
    <template #default="{ row }">
      <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableMeteField], dp: row.accountingPrecision }" />
      /
      <span v-empty-text v-to-fixed="row.accountingPrecision">{{ row.mete }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

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
  operableQuantityField: {
    // 可操作数量字段
    type: String,
    default: 'operableQuantity'
  },
  operableMeteField: {
    // 可操作核算量量字段
    type: String,
    default: 'operableMete'
  }
})

const mateLabel = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V:
      return '重量（kg）'
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return '核算量'
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

const showMeasureUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible('measureUnit')))
const showAccountingUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible('accountingUnit')))
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible('quantity'))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible('mete'))
</script>

<style lang="scss" scoped>
.operable-number {
  color: green;
}
</style>

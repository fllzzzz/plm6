<template>
  <el-table-column v-if="showThickness" prop="source.thickness" align="center" width="70px" :label="`厚 (${baseUnit.thickness.unit})`">
    <template #default="{ row }">
      <span v-to-fixed="{ val: row.thickness, dp: baseUnit.thickness.precision }" />
    </template>
  </el-table-column>
  <el-table-column v-if="showWidth" prop="width" align="center" width="110px" :label="`宽 (${baseUnit.width.unit})`">
    <template #default="{ row }">
      <span v-empty-text v-to-fixed="{ val: row.width, dp: baseUnit.width.precision }" />
      <template v-if="row.maxWidth < row.width">
        ->
        <span v-empty-text v-to-fixed="{ val: row.maxWidth, dp: baseUnit.width.precision }" class="color-coral" />
      </template>
    </template>
  </el-table-column>
  <el-table-column v-if="showLength" prop="length" align="center" width="110px" :label="`长 (${baseUnit.length.unit})`">
    <template #default="{ row }">
      <span v-empty-text v-to-fixed="{ val: row.length, dp: baseUnit.length.precision }" />
      <template v-if="row.maxLength < row.length">
        ->
        <span v-empty-text v-to-fixed="{ val: row.maxLength, dp: baseUnit.length.precision }" class="color-coral"></span>
      </template>
    </template>
  </el-table-column>
</template>

<script setup>
import { computed, defineProps } from 'vue'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  }
})
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(props.basicClass)

const showLength = computed(() => {
  return [rawMatClsEnum.STEEL_PLATE.V, rawMatClsEnum.SECTION_STEEL.V].includes(props.basicClass)
})

const showWidth = computed(() => {
  return rawMatClsEnum.STEEL_PLATE.V === props.basicClass
})

const showThickness = computed(() => {
  return rawMatClsEnum.STEEL_PLATE.V === props.basicClass
})
</script>

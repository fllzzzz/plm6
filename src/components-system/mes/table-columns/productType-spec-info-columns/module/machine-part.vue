<template>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('length')"
    :show-overflow-tooltip="true"
    prop="length"
    :label="`长度\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ARTIFACT_L__MM'" v-empty-text>{{ row.length }}</span>
    </template>
  </el-table-column>
  <slot name="quantity" />
  <el-table-column
    v-if="isBlank(columns) || columns.visible('netWeight')"
    :show-overflow-tooltip="true"
    prop="netWeight"
    :label="`单净重\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_WT__KG'" v-empty-text>{{ row.netWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('grossWeight')"
    :show-overflow-tooltip="true"
    prop="grossWeight"
    :label="`单毛重\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_WT__KG'" v-empty-text>{{ row.grossWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('totalNetWeight')"
    :show-overflow-tooltip="true"
    prop="totalNetWeight"
    :label="`总净重\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_WT__KG'" v-empty-text>{{ row.totalNetWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('totalGrossWeight')"
    :show-overflow-tooltip="true"
    prop="totalGrossWeight"
    :label="`总毛重\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_WT__KG'" v-empty-text>{{ row.totalGrossWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('surfaceArea')"
    :show-overflow-tooltip="true"
    prop="surfaceArea"
    :label="`面积\n(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_AREA__M2'" v-empty-text>{{ row.surfaceArea }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { isBlank } from '@/utils/data-type'

defineProps({
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  },
  fixedWidth: {
    type: Boolean
  }
})
</script>

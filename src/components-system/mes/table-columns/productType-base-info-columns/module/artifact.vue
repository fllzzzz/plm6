<template>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('name')"
    :show-overflow-tooltip="true"
    prop="name"
    label="名称"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <slot name="namePrefix" :row="row"></slot>
      <span v-empty-text>{{ row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('serialNumber')"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="编号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('specification')"
    :show-overflow-tooltip="true"
    prop="specification"
    label="规格"
    :width="fixedWidth ? '140px' : ''"
    :min-width="!fixedWidth ? '140px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.specification }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('length')"
    :show-overflow-tooltip="true"
    prop="length"
    label="长度(mm)"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ARTIFACT_L__MM'" v-empty-text>{{ row.length }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('material')"
    :show-overflow-tooltip="true"
    prop="material"
    label="材质"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.material }}</span>
    </template>
  </el-table-column>
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
    v-if="isBlank(columns) || columns.visible('drawingNumber')"
    :show-overflow-tooltip="true"
    prop="drawingNumber"
    label="图号"
    :width="fixedWidth ? '140px' : ''"
    :min-width="!fixedWidth ? '140px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.drawingNumber }}</span>
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
  <el-table-column
    v-if="isBlank(columns) || columns.visible('remark')"
    :show-overflow-tooltip="true"
    prop="remark"
    label="备注"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.remark }}</span>
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
    type: Boolean,
    default: false
  }
})
</script>

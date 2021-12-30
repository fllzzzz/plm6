<template>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('thickness')"
    :show-overflow-tooltip="true"
    prop="thickness"
    :label="`板厚\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ENCLOSURE_T__MM'" v-empty-text>{{ row.thickness }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('width')"
    :show-overflow-tooltip="true"
    prop="width"
    :label="`有效宽度\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ENCLOSURE_W__MM'" v-empty-text>{{ row.width }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('brand')"
    :show-overflow-tooltip="true"
    prop="brand"
    label="芯材品牌"
    align="center"
    width="100px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.brand }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('type')"
    :show-overflow-tooltip="true"
    prop="type"
    label="芯材种类"
    width="100px"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.type }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('capacity')"
    :show-overflow-tooltip="true"
    prop="capacity"
    align="center"
    :label="`芯材容重\n(kg/m³)`"
    width="100px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_UNIT__KG_M3'" v-empty-text>{{ row.capacity }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('length')"
    :show-overflow-tooltip="true"
    prop="length"
    :label="`单长\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ENCLOSURE_L__MM'" v-empty-text>{{ row.length }}</span>
    </template>
  </el-table-column>
  <slot name="quantity" />
  <el-table-column
    v-if="isBlank(columns) || columns.visible('totalArea')"
    :show-overflow-tooltip="true"
    prop="totalArea"
    :label="`总面积\n(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_AREA__M2'" v-empty-text>{{ row.totalArea }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('totalLength')"
    :show-overflow-tooltip="true"
    prop="totalLength"
    :label="`总长度\n(m)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'MES_ENCLOSURE_L__M'" v-empty-text>{{ row.totalLength }}</span>
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

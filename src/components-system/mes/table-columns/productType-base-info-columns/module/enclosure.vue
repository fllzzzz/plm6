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
    v-if="isBlank(columns) || columns.visible('color')"
    :show-overflow-tooltip="true"
    prop="color"
    label="颜色"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.color }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('material')"
    :show-overflow-tooltip="true"
    prop="material"
    label="材质"
    width="100px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.material }}</span>
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
  <el-table-column
    v-if="isBlank(columns) || columns.visible('weight')"
    :show-overflow-tooltip="true"
    prop="weight"
    :label="`重量\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="'COM_WT__KG'" v-empty-text>{{ row.weight }}</span>
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
  }
})
</script>

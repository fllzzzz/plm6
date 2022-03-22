<template>
  <el-table-column
    v-if="!unShowField.includes('category') && (isBlank(columns) || columns.visible('category'))"
    :show-overflow-tooltip="true"
    prop="category"
    label="类型"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
    align="center"
  >
    <template #default="{ row }">
      <span>{{ row.category }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('quantity') && (isBlank(columns) || columns.visible('quantity'))"
    :show-overflow-tooltip="true"
    prop="quantity"
    label="数量(件)"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.quantity }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalArea') && (isBlank(columns) || columns.visible('totalArea'))"
    :show-overflow-tooltip="true"
    prop="totalArea"
    :label="`总面积${unitNewLine ? '\n' : ''}(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>
        {{ convertUnits(row.totalArea, 'mm2', 'm2', DP.COM_AREA__M2) }}
      </span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalLength') && (isBlank(columns) || columns.visible('totalLength'))"
    :show-overflow-tooltip="true"
    prop="totalLength"
    :label="`总长度${unitNewLine ? '\n' : ''}(m)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>
        {{ convertUnits(row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
      </span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'

import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
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
  },
  unitNewLine: {
    type: Boolean,
    default: true
  },
  // 围护子类型
  category: {
    type: Number
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})
</script>

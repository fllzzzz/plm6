<template>
  <el-table-column
    v-if="!unShowField.includes('name') && !(unShowNameVal & category) && (isBlank(columns) || columns.visible('name'))"
    :show-overflow-tooltip="true"
    prop="name"
    label="名称"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <slot name="namePrefix" :row="row"></slot>
      <span>{{ row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('serialNumber') && !(unShowSNVal & category) && (isBlank(columns) || columns.visible('serialNumber'))"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="编号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template v-if="snClickable" #header>
      <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
        <div style="display: inline-block">
          <span>编号</span>
          <i class="el-icon-info" />
        </div>
      </el-tooltip>
    </template>
    <template #default="{ row }">
      <span v-if="!snClickable">{{ row.serialNumber }}</span>
      <span v-else style="cursor: pointer" @dblclick="drawingPreview(row)">{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('plate') && !(unShowPLVal & category) && (isBlank(columns) || columns.visible('plate'))"
    :show-overflow-tooltip="true"
    prop="plate"
    label="板型"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.plate }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, defineEmits } from 'vue'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import { isBlank } from '@/utils/data-type'

const emit = defineEmits(['drawingPreview'])

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
  snClickable: {
    type: Boolean,
    default: false
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

const unShowNameVal = computed(() => {
  return 0
})

const unShowSNVal = computed(() => {
  return 0
})

const unShowPLVal = computed(() => {
  return mesEnclosureTypeEnum.FOLDING_PIECE.V
})

function drawingPreview(row) {
  emit('drawingPreview', row)
}
</script>

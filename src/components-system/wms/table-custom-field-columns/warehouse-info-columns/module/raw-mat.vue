<template>
  <el-table-column
    v-if="showProject"
    :key="`${field}.project`"
    :prop="`${field}.project`"
    label="项目"
    align="left"
    min-width="120px"
    show-overflow-tooltip
    :fixed="fixed"
  >
    <template #default="{ row }">
      <table-cell-tag v-if="showTransfer && getInfo(row, 'boolTransfer')" name="调拨" type="transfer" :offset="15" />
      {{ getInfo(row, 'project') }}
    </template>
  </el-table-column>
  <el-table-column
    v-if="showMonomer"
    :key="`${field}.monomerName`"
    :prop="`${field}.monomerName`"
    show-overflow-tooltip
    label="单体"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showArea"
    :key="`${field}.areaName`"
    :prop="`${field}.areaName`"
    show-overflow-tooltip
    label="区域"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showWorkshop"
    :key="`${field}.workshopName`"
    :prop="`${field}.workshopName`"
    show-overflow-tooltip
    label="车间"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showWarehouse"
    :key="`${field}.warehouse`"
    :prop="`${field}.warehouse`"
    label="仓库"
    align="left"
    min-width="110px"
    show-overflow-tooltip
    :fixed="fixed"
  >
    <template #default="{ row }">
      <factory-table-cell-tag
        v-if="props.showFactory"
        :id="getInfo(row, 'factory') ? getInfo(row, 'factory.id') : getInfo(row, 'factoryId')"
      />
      <span>{{ getInfo(row, 'warehouse') ? getInfo(row, 'warehouse.name') : getInfo(row, 'warehouse') }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'

const props = defineProps({
  showProject: {
    // 显示项目
    type: Boolean,
    default: false
  },
  showTransfer: {
    // 项目标签 显示调拨
    type: Boolean,
    default: false
  },
  showMonomer: { // 显示单体
    type: Boolean,
    default: false
  },
  showArea: { // 显示区域
    type: Boolean,
    default: false
  },
  showWorkshop: { // 显示车间
    type: Boolean,
    default: false
  },
  showFactory: {
    // 显示工厂
    type: Boolean,
    default: true
  },
  columns: {
    // 用于crud组件的列显隐
    type: Object
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  },
  fixed: {
    // 固定列
    type: String
  }
})

const getInfo = inject('getInfo')

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.warehouse`))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible(`${props.field}.project`)))
const showMonomer = computed(() => props.showMonomer && (isBlank(props.columns) || props.columns.visible(`${props.field}.monomerName`)))
const showArea = computed(() => props.showArea && (isBlank(props.columns) || props.columns.visible(`${props.field}.areaName`)))
const showWorkshop = computed(() => props.showWorkshop && (isBlank(props.columns) || props.columns.visible(`${props.field}.workshopName`)))
</script>

<template>
  <el-table-column
    v-if="showProject"
    key="project"
    prop="project"
    label="项目"
    align="left"
    min-width="120px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <table-cell-tag v-if="showTransfer && row.boolTransfer" name="调拨" type="transfer" :offset="15" />
      {{ row.project }}
    </template>
  </el-table-column>
  <el-table-column
    v-if="showMonomer"
    key="monomerName"
    :show-overflow-tooltip="true"
    prop="monomerName"
    label="单体"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showArea"
    key="areaName"
    :show-overflow-tooltip="true"
    prop="areaName"
    label="区域"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showWorkshop"
    key="workshop.name"
    :show-overflow-tooltip="true"
    prop="workshop.name"
    label="车间"
    align="left"
    min-width="100"
  />
  <el-table-column
    v-if="showWarehouse"
    key="warehouse"
    prop="warehouse"
    label="仓库"
    align="left"
    min-width="110px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <factory-table-cell-tag v-if="props.showFactory" :id="row.factory ? row.factory.id : row.factoryId" />
      {{ row.warehouse ? row.warehouse.name : row.warehouseName }}
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
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
  fixed: {
    // 固定列
    type: String
  }
})

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible('warehouse'))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible('project')))
const showMonomer = computed(() => props.showMonomer && (isBlank(props.columns) || props.columns.visible('monomerName')))
const showArea = computed(() => props.showArea && (isBlank(props.columns) || props.columns.visible('areaName')))
const showWorkshop = computed(() => props.showWorkshop && (isBlank(props.columns) || props.columns.visible('workshop.name')))
</script>

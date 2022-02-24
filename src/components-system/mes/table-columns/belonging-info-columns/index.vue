<template>
  <el-table-column
    v-if="showProject && (isBlank(columns) || columns.visible('project.shortName'))"
    prop="project.shortName"
    :show-overflow-tooltip="true"
    label="所属项目"
    :width="fixedWidth ? '180px' : ''"
    :min-width="!fixedWidth ? '180px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-parse-project="{ project: row.project }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column
    v-if="showMonomer && (isBlank(columns) || columns.visible('monomer.name'))"
    prop="monomer.name"
    :show-overflow-tooltip="true"
    label="单体"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.monomer?.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showArea && (isBlank(columns) || columns.visible('areaDetail.name'))"
    prop="areaDetail.name"
    :show-overflow-tooltip="true"
    label="区域"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.areaDetail?.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showFactory && (isBlank(columns) || columns.visible('factory.name'))"
    prop="factory.name"
    :show-overflow-tooltip="true"
    label="工厂"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.factory?.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showWorkshop && (isBlank(columns) || columns.visible('workshop.name'))"
    prop="workshop.name"
    :show-overflow-tooltip="true"
    label="车间"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.workshop?.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showProductionLine && (isBlank(columns) || columns.visible('productionLine.name'))"
    prop="productionLine.name"
    :show-overflow-tooltip="true"
    label="生产线"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.productionLine?.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showProcess && (isBlank(columns) || columns.visible('processName'))"
    prop="processName"
    :show-overflow-tooltip="true"
    label="工序"
    :width="fixedWidth ? '100px' : ''"
    :min-width="!fixedWidth ? '100px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.processName }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showTeam && (isBlank(columns) || columns.visible('leaderName'))"
    prop="leaderName"
    :show-overflow-tooltip="true"
    label="班组"
    :align="teamAlign"
    :width="fixedWidth ? '100px' : ''"
    :min-width="!fixedWidth ? '100px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.leaderName }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { isBlank } from '@/utils/data-type'

defineProps({
  // 显示项目
  showProject: {
    type: Boolean,
    default: false
  },
  // 显示区域
  showArea: {
    type: Boolean,
    default: false
  },
  // 显示单体
  showMonomer: {
    type: Boolean,
    default: false
  },
  // 显示工厂
  showFactory: {
    type: Boolean,
    default: false
  },
  // 显示车间
  showWorkshop: {
    type: Boolean,
    default: false
  },
  // 显示生产线
  showProductionLine: {
    type: Boolean,
    default: false
  },
  // 显示工序
  showProcess: {
    type: Boolean,
    default: false
  },
  // 显示编组
  showTeam: {
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  },
  teamAlign: {
    type: String,
    default: 'left'
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

<template>
  <el-table-column
    v-if="showProject"
    :key="`${field}.project`"
    :prop="`${field}.project`"
    label="项目"
    align="left"
    min-width="120px"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <table-cell-tag v-if="showTransfer && getInfo(row, 'boolTransfer')" name="调拨" type="transfer" :offset="15" />
      <span v-parse-project="{ project: getInfo(row, 'project'), onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column
    v-if="showWarehouse"
    :key="`${field}.warehouse`"
    :prop="`${field}.warehouse`"
    label="仓库"
    align="left"
    min-width="110px"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <factory-table-cell-tag
        v-if="props.showFactory"
        :id="getInfo(row, 'factory') ? getInfo(row, 'factory.id') : getInfo(row, 'factoryId')"
      />
      <span v-empty-text>{{ getInfo(row, 'warehouse') ? getInfo(row, 'warehouse.name') : getInfo(row, 'warehouse') }}</span>
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
  }
})

const getInfo = inject('getInfo')

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.warehouse`))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible(`${props.field}.project`)))
</script>

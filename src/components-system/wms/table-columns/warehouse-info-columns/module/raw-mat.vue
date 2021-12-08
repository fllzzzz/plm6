<template>
  <el-table-column v-if="showProject" prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
    <template #default="{ row }">
      <table-cell-tag v-if="showTransfer && row.boolTransfer" name="调拨" :color="TAG_TRANSFER_COLOR" :offset="15" />
      <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showWarehouse" prop="warehouse" label="仓库" align="left" min-width="110px">
    <template #default="{ row }">
      <span v-empty-text>{{ typeof row.warehouse === 'object' ? row.warehouse.name : row.warehouse }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { TAG_TRANSFER_COLOR } from '@/settings/config'

import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'
const props = defineProps({
  showProject: { // 显示项目
    type: Boolean,
    default: false
  },
  showTransfer: { // 项目标签 显示调拨
    type: Boolean,
    default: false
  },
  columns: { // 用于crud组件的列显隐
    type: Object
  }
})

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible('warehouse'))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible('project')))

</script>


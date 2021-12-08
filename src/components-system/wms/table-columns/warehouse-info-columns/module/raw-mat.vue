<template>
  <el-table-column v-if="showProject" prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showWarehouse" prop="warehouse" label="仓库" align="left" min-width="90px">
    <template #default="{ row }">
      <span v-empty-text>{{ typeof row.warehouse === 'object' ? row.warehouse.name : row.warehouse }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  showProject: {
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  }
})

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible('warehouse'))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible('project')))

</script>


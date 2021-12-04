<template>
  <el-table-column prop="brand" label="品牌" align="left" min-width="100px" >
    <template #default="{ row }">
          <span v-empty-text>{{ row.brand }}</span>
      </template>
  </el-table-column>
  <el-table-column v-if="props.showProject" prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  showProject: {
    type: Boolean,
    default: false
  }
})
</script>

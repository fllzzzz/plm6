<template>
  <div class="head-container">
  </div>
  <common-table
    v-loading="loading"
    highlight-current-row
    :data="tableData"
    return-source-data
    row-key="id"
    :stripe="false"
    :max-height="maxHeight - 45"
    style="width: 100%"
    @current-change="handleClickChange"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="process.name" :show-overflow-tooltip="true" label="工序" width="80" align="center">
      <template #default="{ row }">
        <span>{{ row.process?.name }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="groups.name" :show-overflow-tooltip="true" label="班组" min-width="100" header-align="center">
      <template #default="{ row }">
        <span>{{ row.workshop?.name }}>{{ row.groups?.name }}-{{row.team?.name}}</span>
      </template>
    </el-table-column>
    <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="产量（件/kg）" width="140" align="center">
      <template #default="{ row }">
        <span>{{ row.inspectionQuantity }}/{{ row.netWeight }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { inject, defineProps, defineEmits } from 'vue'

const tableData = inject('tableData')
const loading = inject('loading')
const emit = defineEmits(['nesting-task-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

function handleClickChange(val) {
  emit('nesting-task-click', val)
}

</script>

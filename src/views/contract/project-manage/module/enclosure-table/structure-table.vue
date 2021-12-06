<template>
  <!-- 结构表格 -->
  <common-table
    :data="tableData"
    border
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="type" :show-overflow-tooltip="true" align="center" label="产品种类">
      <template v-slot="scope">
        <span>{{ scope.row.type }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="techDesc" :show-overflow-tooltip="true" align="center" label="技术要求描述">
      <template v-slot="scope">
        <span>{{ scope.row.techDesc }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="!isShow" label="操作" align="center" fixed="right">
      <template v-slot="scope">
        <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.$index,scope.row)" />
        <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { ref } from 'vue'
import { DP } from '@/settings/config'
const props = defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  isShow: {
    type: Boolean,
    default: true
  }
})

const emit = defineEmits(['edit'])

function deleteRow (index){
  props.tableData.splice(index, 1)
}

function editRow(index, row) {
  emit('edit', row)
  props.tableData.splice(index, 1)
}
</script>

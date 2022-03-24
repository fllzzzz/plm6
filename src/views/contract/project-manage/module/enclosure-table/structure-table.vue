<template>
  <!-- 结构表格 -->
  <common-table
    :data="tableData"
    return-source-data
    :showEmptySymbol="false"
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
    <el-table-column v-if="!isShow" label="操作" align="center">
      <template v-slot="scope">
        <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.$index,scope.row)" />
        <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
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

const techTableData = ref([])

watch(
  () => props.tableData,
  (val) => {
    techTableData.value = props.tableData
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['edit'])

function deleteRow(index) {
  techTableData.value.splice(index, 1)
  // props.tableData.splice(index, 1)
}

function editRow(index, row) {
  emit('edit', row)
  techTableData.value.splice(index, 1)
  // props.tableData.splice(index, 1)
}
</script>

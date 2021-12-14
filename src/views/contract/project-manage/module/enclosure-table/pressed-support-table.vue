<template>
  <!-- 压型楼层板表格 -->
  <common-table
    :data="tableData"
    border
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="板型">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" label="厚度">
      <template v-slot="scope">
        <span>{{ scope.row.thickness? Number(scope.row.thickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plating" :show-overflow-tooltip="true" align="center" label="镀层">
      <template v-slot="scope">
        <span>{{ scope.row.plating }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="usePart" :show-overflow-tooltip="true" align="center" label="使用部位">
      <template v-slot="scope">
        <span>{{ scope.row.usePart }}</span>
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
import { ref, defineProps } from 'vue'
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

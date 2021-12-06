<template>
  <!-- 桁架楼层板表格 -->
  <common-table
    :data="tableData"
    border
  >
    <el-table-column :label="'序号'" type="index" align="center" width="60" />
    <el-table-column prop="code" :show-overflow-tooltip="true" align="center" label="编号">
      <template v-slot="scope">
        <span>{{ scope.row.code }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="high" :show-overflow-tooltip="true" align="center" label="高度">
      <template v-slot="scope">
        <span>{{ scope.row.high }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="firstQuarter" :show-overflow-tooltip="true" align="center" label="上弦">
      <template v-slot="scope">
        <span>{{ scope.row.firstQuarter }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="lastQuarter" :show-overflow-tooltip="true" align="center" label="下弦">
      <template v-slot="scope">
        <span>{{ scope.row.lastQuarter }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="webMember" :show-overflow-tooltip="true" align="center" label="腹杆">
      <template v-slot="scope">
        <span>{{ scope.row.webMember }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="vertical" :show-overflow-tooltip="true" align="center" label="竖向">
      <template v-slot="scope">
        <span>{{ scope.row.vertical }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="level" :show-overflow-tooltip="true" align="center" label="水平">
      <template v-slot="scope">
        <span>{{ scope.row.level }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="basementMembrane" :show-overflow-tooltip="true" align="center" label="底膜">
      <template v-slot="scope">
        <span>{{ scope.row.basementMembrane }}</span>
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

<template>
  <!-- 桁架楼层板表格 -->
  <common-table
    :data="tableData"
    :showEmptySymbol="false"
    border
  >
    <el-table-column :label="'序号'" type="index" align="center" width="60" />
    <el-table-column prop="serialNumber" :show-overflow-tooltip="true" align="center" label="版型">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="basementMembrane" :show-overflow-tooltip="true" align="center" label="底膜(mm)">
      <template v-slot="scope">
        <span>{{ scope.row.basementMembrane }}</span>
      </template>
    </el-table-column>
    <!-- <el-table-column prop="high" :show-overflow-tooltip="true" align="center" label="高度">
      <template v-slot="scope">
        <span>{{ scope.row.high }}</span>
      </template>
    </el-table-column> -->
    <el-table-column prop="effectiveWidth" :show-overflow-tooltip="true" align="center" label="有效宽度">
      <template v-slot="scope">
        <span>{{ scope.row.effectiveWidth }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="firstQuarter" :show-overflow-tooltip="true" align="center" label="上弦筋(φ)">
      <template v-slot="scope">
        <span>{{ scope.row.firstQuarter }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="lastQuarter" :show-overflow-tooltip="true" align="center" label="下弦筋(φ)">
      <template v-slot="scope">
        <span>{{ scope.row.lastQuarter }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="webMember" :show-overflow-tooltip="true" align="center" label="腹杆筋(φ)">
      <template v-slot="scope">
        <span>{{ scope.row.webMember }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="vertical" :show-overflow-tooltip="true" align="center" label="竖向筋(φ)">
      <template v-slot="scope">
        <span>{{ scope.row.vertical }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="level" :show-overflow-tooltip="true" align="center" label="水平筋(φ)">
      <template v-slot="scope">
        <span>{{ scope.row.level }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="weightMeter" :show-overflow-tooltip="true" align="center" label="米重(kg/m)">
      <template v-slot="scope">
        <span>{{ scope.row.weightMeter }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" :show-overflow-tooltip="true" align="center" label="数量(m)">
      <template v-slot="scope">
        <span>{{ scope.row.quantity }}</span>
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

<template>
  <!-- 压型楼层板表格 -->
  <common-table
    :data="tableData"
    border
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="版型">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="展宽(mm)">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="有效宽(mm)">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="版型">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="mode" :show-overflow-tooltip="true" align="center" label="类型">
      <template v-slot="scope">
        <span>{{ scope.row.mode }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" label="板厚(mm)">
      <template v-slot="scope">
        <span>{{ scope.row.thickness }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plating" :show-overflow-tooltip="true" align="center" label="屈服强度">
      <template v-slot="scope">
        <span>{{ scope.row.plating }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="brand" :show-overflow-tooltip="true" align="center" label="品牌">
      <template v-slot="scope">
        <span>{{ scope.row.brand }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plating" :show-overflow-tooltip="true" align="center" label="镀层(g)">
      <template v-slot="scope">
        <span>{{ scope.row.plating }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" :show-overflow-tooltip="true" align="center" label="数量(m)">
      <template v-slot="scope">
        <span>{{ scope.row.quantity }}</span>
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

const emit = defineEmits(['edit'])

watch(
  () => props.tableData,
  (val) => {
    techTableData.value = props.tableData
  },
  { deep: true, immediate: true }
)

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

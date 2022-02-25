<template>
  <common-table class="list-table" v-bind="$attrs" ref="tableRef" :data="list" :max-height="props.maxHeight">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column label="钢材种类" key="classifyName" prop="classifyName" align="center" width="100" />
    <el-table-column label="材质" key="material" prop="material" align="center" width="100" />
    <el-table-column label="厚度/规格" key="specification" prop="specification" align="center" />
    <el-table-column label="清单量（kg）" key="listMete" prop="listMete" align="center" width="110" />
    <el-table-column label="备料量（kg）" key="preparationMete" prop="preparationMete" align="center" width="110" />
    <el-table-column label="差值（kg）" key="diff" prop="diff" align="center" width="100" />
  </common-table>
</template>

<script setup>
import { ref, defineProps } from 'vue'

const props = defineProps({
  maxHeight: {
    type: Number,
    default: 600
  },
  list: { // 技术清单汇总列表
    type: Array,
    default: () => []
  }
})

// 表格ref
const tableRef = ref()
// 技术清单汇总列表
// const list = ref([])
</script>

<style scoped>
.list-table {
  width:800px;
}
</style>

<template>
  <common-table class="list-table" v-bind="$attrs" ref="tableRef" :data="list" :data-format="columnsDataFormat" :height="props.height">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column label="钢材种类" key="steelClassifyConfName" prop="steelClassifyConfName" align="center" width="100" />
    <el-table-column label="材质" key="material" prop="material" align="center" width="100" />
    <el-table-column label="厚度/规格" key="specification" prop="specification" align="center" />
    <el-table-column label="清单量（kg）" key="listMete" prop="listMete" align="center" width="110" />
    <el-table-column label="备料量（kg）" key="preparationMete" prop="preparationMete" align="center" width="110">
      <template #default="{ row: { sourceRow: row } }">
        <span v-if="techPrepMeteKV[row.id]" v-to-fixed="{ val: techPrepMeteKV[row.id].preparation, k: 'COM_WT__KG' }" />
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column label="差值（kg）" key="diff" prop="diff" align="center" width="100">
      <template #default="{ row: { sourceRow: row } }">
        <span
          v-if="techPrepMeteKV[row.id]"
          :class="techPrepMeteKV[row.id].isEnough ? 'over-text' : 'not-over-text'"
          v-to-fixed="{ val: techPrepMeteKV[row.id].diff, k: 'COM_WT__KG' }"
          v-prefix="techPrepMeteKV[row.id].isEnough && techPrepMeteKV[row.id].diff !== 0 ? '+' : ''"
        />
        <span v-else>-</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { STEEL_BASE_UNIT } from '@/settings/config'
import { ref, defineProps } from 'vue'

const props = defineProps({
  height: {
    type: Number,
    default: 250
  },
  list: {
    // 技术清单汇总列表
    type: Array,
    default: () => []
  },
  techPrepMeteKV: {
    type: Object,
    default: () => ({})
  }
})

// 表格ref
const tableRef = ref()
// 列格式转换
const columnsDataFormat = [['listMete', ['to-fixed', STEEL_BASE_UNIT.weight.precision]]]
// 技术清单汇总列表
// const list = ref([])
</script>

<style scoped>
.list-table {
  width: 800px;
}
</style>

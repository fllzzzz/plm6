<template>
  <div class="app-container">
    <div v-show="!processList.id" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
    <div v-show="processList.id" style="width: 100%">
      <common-table
        ref="tableRef"
        :data="transformTab === processMaterialListTypeEnum.MACHINE_PART.V ? partProcessList : ArtifactProcessList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        highlight-current-row
        row-key="projectId"
        style="width: 100%; cursor: pointer"
        @current-change="handleCurrenChange"
      >
        <el-table-column align="center" key="process" prop="process" :show-overflow-tooltip="true" label="工序">
          <template v-slot="scope">
            <el-icon :size="20" style="top: 5px; color: red"><BellFilled /></el-icon>
            <span>{{ scope.row.process }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="complete" prop="complete" :show-overflow-tooltip="true" label="进度" min-width="100px">
          <template v-slot="scope">
            <el-progress :text-inside="true" stroke-linecap="square" :stroke-width="22" :percentage="scope.row.complete" status="success" />
          </template>
        </el-table-column>
        <el-table-column align="center" key="task" prop="task" :show-overflow-tooltip="true" label="任务（件/kg）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="finish" prop="finish" :show-overflow-tooltip="true" label="完成（件/kg）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <production-line-detail v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { ref, defineProps } from 'vue'
import { BellFilled } from '@element-plus/icons'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import productionLineDetail from '../production-line-detail/index.vue'

defineProps({
  processList: {
    type: Object,
    default: () => {}
  },
  transformTab: {
    type: Number,
    default: undefined
  }
})
const ArtifactProcessList = [
  {
    process: '组立',
    type: 1,
    complete: 40,
    quantity: 25,
    weight: 1000,
    planDate: 1630000,
    currentDate: 1620000,
    productionLineList: [
      { workshop: '一车间', productionLine: '一线' },
      { workshop: '二车间', productionLine: '二线' }
    ]
  },
  { process: '埋弧', type: 2, complete: 60, quantity: 59, weight: 1000, planDate: 1630000, currentDate: 1620000 },
  { process: '总装', type: 4, complete: 27, quantity: 70, weight: 1000, planDate: 20000000, currentDate: 1620000 },
  { process: '焊接', type: 16, complete: 45, quantity: 45, weight: 1000, planDate: 20000000, currentDate: 1620000 }
]
const partProcessList = [
  { process: '切割', type: 8, complete: 40, quantity: 25, weight: 1000, planDate: 1630000, currentDate: 1620000 },
  { process: '钻孔', type: 8, complete: 60, quantity: 59, weight: 1000, planDate: 1630000, currentDate: 1620000 }
]

const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleCurrenChange(row) {
  drawerVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

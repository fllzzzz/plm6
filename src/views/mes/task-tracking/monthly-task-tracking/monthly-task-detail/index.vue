<template>
  <div class="app-container">
    <common-table
      ref="tableRef"
      :data="monthlyData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%; cursor: pointer"
      @current-change="handleProjectDetail"
    >
      <el-table-column align="center" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目">
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="monomer" prop="monomer" :show-overflow-tooltip="true" label="包含单体">
        <template v-slot="scope">
          <template v-for="item in scope.row.monomer" :key="item">
            <span>{{ item }}/</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column align="center" key="area" prop="area" :show-overflow-tooltip="true" label="包含区域">
        <template v-slot="scope">
          <template v-for="item in scope.row.area" :key="item">
            <span>{{ item }}/</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column align="center" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="排产量（件/吨）">
        <template v-slot="scope">
          <span>{{ scope.row.totalQuantity }}/{{ scope.row.totalWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="完成率">
        <template v-slot="scope">
          <span>
            <el-progress
              :text-inside="true"
              stroke-linecap="square"
              :stroke-width="22"
              :percentage="scope.row.finishedRate"
              status="success"
            />
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="实际完成（件/吨）">
        <template v-slot="scope">
          <span>{{ scope.row.actualQuantity }}/{{ scope.row.actualWeight }}</span>
        </template>
      </el-table-column>
    </common-table>
    <project-detail v-model:visible="drawerVisible" :detail-data="detailData"/>
  </div>
</template>
<script setup>
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import projectDetail from '../project-detail/index.vue'

// const props = defineProps({
//   monthlyData: {
//     type: Object,
//     default: () => {}
//   }
// })
// const monthlyData = ref([])
const monthlyData = [
  { projectName: '浙江国家大学科技园', monomer: ['一号楼', '二号楼'], area: ['第一批', '第二批'], totalQuantity: 200, totalWeight: 1000, finishedRate: 36, actualQuantity: 100, actualWeight: 1000 }
]
const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleProjectDetail(row) {
  console.log(row, 'row')
  drawerVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

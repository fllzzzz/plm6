<template>
  <div class="app-container">
    <div v-show="!props.monthlyData.month" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
    <div v-show="props.monthlyData.month" style="width: 100%">
      <common-table
        ref="tableRef"
        :data="monthlyList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        highlight-current-row
        row-key="projectId"
        style="width: 100%; cursor: pointer"
        @row-click="handleProjectDetail"
      >
        <el-table-column type="index" label="序号" key="index" align="center" width="60px" />
        <el-table-column key="project.shortName" prop="project" :show-overflow-tooltip="true" label="项目" min-width="150px">
          <template v-slot="scope">
            <span>{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="monomer" prop="monomer" :show-overflow-tooltip="true" label="包含单体">
          <template v-slot="scope">
            <template v-for="item in scope.row.monomerDO" :key="item">
              <span v-if="scope.row.monomerDO.length > 1">{{ item.name }}/</span>
              <span v-else-if="scope.row.monomerDO.length === 1">{{ item.name }}</span>
              <span v-else>-</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column align="center" key="area" prop="area" :show-overflow-tooltip="true" label="包含区域">
          <template v-slot="scope">
            <template v-for="item in scope.row.areaDO" :key="item">
              <span v-if="scope.row.areaDO.length > 1">{{ item.name }}/</span>
              <span v-else-if="scope.row.areaDO.length === 1">{{ item.name }}</span>
              <span v-else>-</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="排产量（件/吨）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ ((scope.row.mete) / 1000).toFixed(DP.COM_WT__T) }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="rate" prop="rate" :show-overflow-tooltip="true" label="完成率" width="160px">
          <template v-slot="scope">
            <span>
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="22"
                :percentage="((scope.row.completeQuantity / scope.row.quantity) * 100 ).toFixed(2)"
                status="success"
              />
            </span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          label="实际完成（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}/{{ ((scope.row.completeMete) / 1000).toFixed(DP.COM_WT__T) }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <project-detail v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { monthlyProject } from '@/api/mes/task-tracking/monthly-task-tracking.js'
import { ref, defineProps, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import projectDetail from '../project-detail/index.vue'
import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'
import moment from 'moment'

const props = defineProps({
  monthlyData: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object
  }
})

const monthlyList = ref([])
const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

watch(
  () => props.monthlyData.month,
  (val) => {
    if (val) {
      fetchMonthly()
    }
  }
)

async function fetchMonthly() {
  try {
    const time = moment().set('month', props.monthlyData.month - 1)._d
    const current = new Date(time.getFullYear() + '-' + props.monthlyData.month).getTime()
    // console.log(current);
    // console.log(new Date(current).getTime());
    const data = await monthlyProject({
      ...props.query,
      dateTime: current
    })
    monthlyList.value = data
  } catch (e) {
    console.log('获取当前月的项目数据失败', e)
  }
}

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

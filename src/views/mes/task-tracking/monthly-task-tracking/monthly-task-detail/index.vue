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
        <el-table-column align="left" key="monomer" prop="monomer" :show-overflow-tooltip="true" label="包含单体">
          <template v-slot="scope">
            <template v-for="item in scope.row.monomerDO" :key="item">
              <span v-if="scope.row.monomerDO.length > 1">{{ item.name }}/</span>
              <span v-else-if="scope.row.monomerDO.length === 1">{{ item.name }}</span>
              <span v-else>-</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column align="left" key="area" prop="area" :show-overflow-tooltip="true" label="包含区域">
          <template v-slot="scope">
            <template v-for="item in scope.row.areaDO" :key="item">
              <span v-if="scope.row.areaDO.length > 1">{{ item.name }}/</span>
              <span v-else-if="scope.row.areaDO.length === 1">{{ item.name }}</span>
              <span v-else>-</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column align="center" key="list" prop="list" :show-overflow-tooltip="true" label="排产量（件/吨）">
          <template v-slot="scope">
            <span>{{
              props.weightStatus === weightTypeEnum.NET.V
                ? scope.row.quantity + '/' + (scope.row.netWeight / 1000).toFixed(DP.COM_WT__KG)
                : scope.row.quantity + '/' + (scope.row.grossWeight / 1000).toFixed(DP.COM_WT__KG)
            }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="rate" prop="rate" :show-overflow-tooltip="true" label="完成率" width="160px">
          <template v-slot="scope">
            <span>
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="22"
                :percentage="((scope.row.completeQuantity / scope.row.quantity) * 100).toFixed(2)"
                status="success"
              />
            </span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="complete"
          prop="complete"
          :show-overflow-tooltip="true"
          label="实际完成（件/吨）"
        >
          <template v-slot="scope">
            <span>{{
              props.weightStatus === weightTypeEnum.NET.V
                ? scope.row.completeQuantity + '/' + (scope.row.completeNetWeight / 1000).toFixed(DP.COM_WT__KG)
                : scope.row.completeQuantity + '/' + (scope.row.completeGrossWeight / 1000).toFixed(DP.COM_WT__KG)
            }}</span>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </div>
    <project-detail v-model:visible="drawerVisible" :dateTime="props.monthlyData?.month" :detail-data="detailData" />
  </div>
</template>
<script setup>
import { monthlyProject } from '@/api/mes/task-tracking/monthly-task-tracking.js'
import { ref, defineProps, watch } from 'vue'
import { weightTypeEnum } from '@enum-ms/common'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import projectDetail from '../project-detail/index.vue'
import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'

const props = defineProps({
  monthlyData: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object
  },
  weightStatus: {
    type: Number
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
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchMonthly })

async function fetchMonthly() {
  let _list = []
  try {
    const { content = [], totalElements } = await monthlyProject({
      ...props.query,
      dateTime: new Date(props.monthlyData.month).getTime(),
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取当前月的项目数据失败', e)
  } finally {
    monthlyList.value = _list
  }
}

function handleProjectDetail(row) {
  drawerVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>

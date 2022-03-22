<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :stripe="false"
        :span-method="objectSpanMethod"
        :showEmptySymbol="false"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('type')" align="center" key="type" prop="type" :show-overflow-tooltip="true" label="计划类型" width="180" />
        <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="类型" width="180" >
          <template v-slot="scope">
            <el-tag :type="scope.row.showType===2?'danger':''">{{ scope.row.className }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('rateProgress')"
          key="rateProgress"
          prop="rateProgress"
          :show-overflow-tooltip="true"
          label="完成进度"
          min-width="320"
        >
          <template v-slot="scope">
            <div v-if="scope.row.showType===1">{{`计划用时${scope.row.beforeVal || '-'}天|已用时${scope.row.afterVal || '-'}天`}}</div>
            <div v-if="scope.row.showType===2">{{`总量${scope.row.beforeVal.toFixed(unitInfo.decimal)}${unitInfo.unit}|已完成${scope.row.afterVal.toFixed(unitInfo.decimal)}${unitInfo.unit}`}}</div>
            <el-progress
              v-if="isNotBlank(scope.row.completionRate)"
              :text-inside="true"
              :stroke-width="26"
              :percentage="scope.row.completionRate"
              :status="scope.row.showType===2?'exception':''"
            />
          </template>
        </el-table-column>
         <el-table-column
          v-if="columns.visible('rate')"
          key="rate"
          prop="rate"
          :show-overflow-tooltip="true"
          label="完成率"
          width="280"
        >
          <template v-slot="scope">
            <el-tag :type="scope.row.showType===2?'danger':''">{{`${scope.row.completionRate}%`}}</el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
    <template v-else>
      <div style="color: red; font-size: 14px">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-progress'
import { ref, watch, computed } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { overallPlanStatusEnum, areaPlanTypeEnum } from '@enum-ms/plan'
import { planProgressListPM as permission } from '@/page-permission/plan'
import { dateDifferenceReduce } from '@/utils/date'
import { DP } from '@/settings/config'
import { isNotBlank } from '@/utils/data-type'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '计划跟踪',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaProductType'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)
const unitInfo = computed(() => {
  const data = {}
  if (crud.query.areaProductType === TechnologyTypeAllEnum.STRUCTURE.V) {
    // 显示t，这块只保留两位小数
    data.decimal = DP.COM_WT__KG
    data.unit = 't'
  } else {
    data.decimal = DP.MES_ENCLOSURE_L__M
    data.unit = 'm'
  }
  return data
})
const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-progress',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    if (rowIndex % 2 === 0) {
      return {
        rowspan: 2,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  const handleData = []
  const list = data.data.content || []
  list.forEach(i => {
    const currentDate = new Date().getTime()
    const plannedDays = i.startDate && i.endDate ? dateDifferenceReduce(i.startDate, i.endDate) : 0
    const completeDays = i.status === overallPlanStatusEnum.PROCESS.V || !i.completeDate ? currentDate : i.endDate
    const actualDays = i.startDate <= currentDate ? dateDifferenceReduce(i.startDate, completeDays) : 0
    const dayData = {
      monomerId: i.monomerId,
      areaProductType: i.areaProductType,
      type: areaPlanTypeEnum.VL[i.type],
      className: '时间',
      classType: null,
      beforeVal: plannedDays,
      afterVal: actualDays,
      completionRate: plannedDays ? Number((actualDays / plannedDays * 100).toFixed(1)) : 0,
      completionColor: '#1890ff',
      showType: 1
    }
    const taskData = {
      monomerId: i.monomerId,
      areaProductType: i.areaProductType,
      type: areaPlanTypeEnum.VL[i.type],
      className: '任务',
      classType: 'danger',
      beforeVal: i.mete || 0,
      afterVal: i.completedMete || 0,
      completionRate: i.mete && i.completedMete ? Number((i.completedMete / i.mete * 100).toFixed(1)) : 0,
      completionColor: '#ff4949',
      showType: 2
    }
    handleData.push(dayData)
    handleData.push(taskData)
  })
  data.data.content = handleData
  return data
}
</script>
<style lang="scss" scoped>
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
::v-deep(.el-progress-bar__inner){
  text-align: center;
  max-width: 100%;
}
// .progress-left {
//   font-size: 12px;
//   position: absolute;
//   z-index: 200;
//   top: 50%;
//   transform: translateY(-50%);
//   left: 0;
// }
// .progress-right {
//   font-size: 12px;
//   position: absolute;
//   z-index: 200;
//   top: 50%;
//   transform: translateY(-50%);
//   right: 50px;
// }
</style>

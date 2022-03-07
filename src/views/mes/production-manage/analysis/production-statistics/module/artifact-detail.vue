<template>
  <common-drawer
    ref="drawerRef"
    :title="`${reportEnum.VL[reportType]}统计明细-构件`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #content>
      <common-table
        v-loading="tableLoading"
        :summary-method="getSummaries"
        show-summary
        v-if="drawerVisible"
        :data="list"
        :max-height="maxHeight"
        row-key="rowId"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="200">
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="monomer.name" prop="monomer.name" :show-overflow-tooltip="true" label="单体">
          <template v-slot="scope">
            <span>{{ scope.row.monomer.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="totalQuantity"
          prop="totalQuantity"
          :show-overflow-tooltip="true"
          :label="`任务总数`"
          align="center"
          width="90"
        >
          <template v-slot="scope">
            <span>{{ scope.row.totalQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalMete" prop="totalMete" :show-overflow-tooltip="true" :label="`任务总量`" align="center" width="90">
          <template v-slot="scope">
            <span>{{ scope.row.totalMete }}</span>
          </template>
        </el-table-column>
        <template v-if="reportType & reportEnum.COMPLETE.V">
          <el-table-column
            key="completeQuantity"
            prop="completeQuantity"
            :show-overflow-tooltip="true"
            :label="`完成总数`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="completeMete"
            prop="completeMete"
            :show-overflow-tooltip="true"
            :label="`完成总量`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.completeMete }}</span>
            </template>
          </el-table-column>
        </template>
        <template v-if="reportType & reportEnum.IN_PRODUCTION.V">
          <el-table-column
            key="inProductionQuantity"
            prop="inProductionQuantity"
            :show-overflow-tooltip="true"
            :label="`在制品总数`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.inProductionQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="inProductionMete"
            prop="inProductionMete"
            :show-overflow-tooltip="true"
            :label="`在制品总量`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.inProductionMete }}</span>
            </template>
          </el-table-column>
        </template>
        <template v-if="reportType & reportEnum.UN_PRODUCTION.V">
          <el-table-column
            key="unProductionQuantity"
            prop="unProductionQuantity"
            :show-overflow-tooltip="true"
            :label="`未生产总数`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.unProductionQuantity }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="unProductionMete"
            prop="unProductionMete"
            :show-overflow-tooltip="true"
            :label="`未生产总量`"
            align="center"
            width="90"
          >
            <template v-slot="scope">
              <span>{{ scope.row.unProductionMete }}</span>
            </template>
          </el-table-column>
        </template>
        <el-table-column
          v-if="reportType & reportEnum.IN_PRODUCTION.V"
          key="processSequence"
          prop="processSequence"
          :show-overflow-tooltip="true"
          label="【工序 │ 数量】"
          min-width="400px"
        >
          <template v-slot="scope">
            <span v-html="scope.row.processSequence" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getCompleteDetail, getInProductionDetail, getUnProductionDetail } from '@/api/mes/production-manage/analysis/production-statistics'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
// import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'
import { tableSummary } from '@/utils/el-extra'
import { inProductionDetailReportEnum as reportEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  reportType: {
    type: Number,
    default: undefined
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const query = inject('query')
const tableLoading = ref(false)
const list = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

function getSummaries(param) {
  return tableSummary(param, { props: ['totalQuantity', 'totalMete'] })
}

async function fetchList() {
  try {
    tableLoading.value = true
    let detail
    switch (props.reportType) {
      case reportEnum.COMPLETE.V:
        detail = getCompleteDetail
        break
      case reportEnum.IN_PRODUCTION.V:
        detail = getInProductionDetail
        break
      case reportEnum.UN_PRODUCTION.V:
        detail = getUnProductionDetail
        break
      default:
        break
    }
    if (!detail) return
    const { artifactDetailsAnalysisDTOList } = await detail(query)
    list.value = artifactDetailsAnalysisDTOList.map((v, i) => {
      v.rowId = i + '' + Math.random()
      v.totalQuantity = v.taskQuantity
      v.totalMete = toFixed(v.taskNetWeight, DP.COM_WT__KG)
      v.completeMete = v.completeNetWeight && toFixed(v.completeNetWeight, DP.COM_WT__KG)
      v.inProductionMete = v.inProductionNetWeight && toFixed(v.inProductionNetWeight, DP.COM_WT__KG)
      v.unProductionMete = v.unProductionNetWeight && toFixed(v.unProductionNetWeight, DP.COM_WT__KG)
      v.processSequence =
        v.processSummaryList &&
        v.processSummaryList
          .map((v) => {
            return `<span>【${v.name} │ <span style="color: #67C23A;">${v.inspectionQuantity}</span>】</span>`
          })
          .join('<span> </span>')
      return v
    })
  } catch (error) {
    console.log('分组获取在制品统计', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

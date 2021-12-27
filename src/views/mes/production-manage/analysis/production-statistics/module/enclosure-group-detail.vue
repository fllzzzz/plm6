<template>
  <common-drawer ref="drawerRef" title="生产统计-围护" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="80%">
    <template #content>
      <common-table
        v-loading="tableLoading"
        :summary-method="getSummaries"
        show-summary
        :data="list"
        :max-height="maxHeight"
        id="groupEnclosureTable"
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
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="taskQuantity"
          prop="taskQuantity"
          :show-overflow-tooltip="true"
          :label="`任务总数(${showUnit})`"
          align="center"
          width="90"
        >
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="taskMete"
          prop="taskMete"
          :show-overflow-tooltip="true"
          :label="`任务总数(${showUnit})`"
          align="center"
          width="120"
        >
          <template v-slot="scope">
            <span>{{ scope.row.taskMete }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          :label="`已完成(${showUnit})`"
          align="center"
          width="90"
        >
          <template #header>
            <span>已完成({{ showUnit }})</span>
            <common-button
              size="mini"
              type="primary"
              style="margin-left: 5px"
              icon="el-icon-view"
              @click="handleHeaderClick"
            ></common-button>
          </template>
          <template v-slot="scope">
            <span class="tc-success">{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="completeMete"
          prop="completeMete"
          :show-overflow-tooltip="true"
          :label="`已完成(${showUnit})`"
          align="center"
          width="120"
        >
          <template v-slot="scope">
            <span class="tc-success">{{ scope.row.completeMete }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="inProductionQuantity"
          prop="inProductionQuantity"
          :show-overflow-tooltip="true"
          :label="`在制品(${showUnit})`"
          align="center"
          width="90"
        >
          <template #header>
            <span>在制品({{ showUnit }})</span>
            <common-button
              size="mini"
              type="primary"
              style="margin-left: 5px"
              icon="el-icon-view"
              @click="handleHeaderClick"
            ></common-button>
          </template>
          <template v-slot="scope">
            <span class="tc-warning">{{ scope.row.inProductionQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="inProductionMete"
          prop="inProductionMete"
          :show-overflow-tooltip="true"
          :label="`在制品(${showUnit})`"
          align="center"
          width="120"
        >
          <template v-slot="scope">
            <span class="tc-warning">{{ scope.row.inProductionMete }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="unProducedQuantity"
          prop="unProducedQuantity"
          :show-overflow-tooltip="true"
          :label="`未生产(${showUnit})`"
          align="center"
          width="90"
        >
          <template #header>
            <span>未生产({{ showUnit }})</span>
            <common-button
              size="mini"
              type="primary"
              style="margin-left: 5px"
              icon="el-icon-view"
              @click="handleHeaderClick"
            ></common-button>
          </template>
          <template v-slot="scope">
            <span class="tc-danger">{{ scope.row.unProducedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="unProducedMete"
          prop="unProducedMete"
          :show-overflow-tooltip="true"
          :label="`未生产(${showUnit})`"
          align="center"
          width="120"
        >
          <template v-slot="scope">
            <span class="tc-danger">{{ scope.row.unProducedMete }}</span>
          </template>
        </el-table-column>
      </common-table>
      <m-detail v-model:visible="detailVisible"></m-detail>
    </template>
  </common-drawer>
</template>

<script setup>
import { getByGroup as detail } from '@/api/mes/production-manage/analysis/production-statistics'
import { defineProps, defineEmits, ref, watch, inject, nextTick } from 'vue'

import { DP } from '@/settings/config'
// import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import mDetail from './enclosure-detail'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
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

const showUnit = '件/m'
const query = inject('query')
const tableLoading = ref(false)
const list = ref([])

// 设置表头合并
function setColSpan() {
  // 获取表头的所有单元格
  const table = document.getElementById('groupEnclosureTable')
  const x = table.getElementsByClassName('el-table__header')[0].rows[0].cells
  const needSetSpan = [5, 7, 9, 11]
  for (let i = 0; i < needSetSpan.length; i++) {
    x[needSetSpan[i]].colSpan = 2
    x[needSetSpan[i] + 1].style.display = 'none'
  }
}

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
      init()
    }
  },
  { immediate: true }
)

function init() {
  nextTick(() => {
    setColSpan()
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index > 4) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
    }
  })
  return sums
}

const detailVisible = ref(false)

function handleHeaderClick() {
  detailVisible.value = true
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { enclosureAnalysisList } = await detail(query)
    list.value = enclosureAnalysisList.map((v) => {
      v.taskMete = convertUnits(v.taskLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
      v.completeMete = convertUnits(v.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
      v.inProductionMete = convertUnits(v.inProductionLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
      v.unProducedQuantity = v.taskQuantity - v.completeQuantity - v.inProductionQuantity
      v.unProducedMete = convertUnits(v.taskMete - v.completeMete - v.inProductionMete, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
      return v
    })
  } catch (error) {
    console.log('分组获取生产统计', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
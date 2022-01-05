<template>
  <common-drawer ref="drawerRef" title="生产统计明细-构件" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="80%">
    <template #content>
      <common-table
        v-loading="tableLoading"
        :summary-method="getSummaries"
        show-summary
        v-if="drawerVisible"
        :data="list"
        :max-height="maxHeight"
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
        <el-table-column
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
import { getDetail as detail } from '@/api/mes/production-manage/analysis/production-statistics'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
// import { convertUnits } from '@/utils/convert/unit'
import { projectNameFormatter } from '@/utils/project'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

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
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 6 && index === 7) {
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

async function fetchList() {
  try {
    tableLoading.value = true
    const { artifactDetailsAnalysisDTOList } = await detail(query)
    list.value = artifactDetailsAnalysisDTOList.map((v) => {
      v.totalQuantity = v.taskQuantity
      v.totalMete = toFixed(v.taskNetWeight, DP.COM_WT__KG)
      v.processSequence = v.processSummaryList
        .map((v) => {
          return `<span>【${v.name} │ <span style="color: #67C23A;">${v.completeQuantity}</span>】</span>`
        })
        .join('<span>→</span>')
      return v
    })
  } catch (error) {
    console.log('分组获取生产统计', error)
  } finally {
    tableLoading.value = false
  }
}
</script>

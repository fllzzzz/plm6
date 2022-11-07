<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="80%"
    :title="`项目详情`"
    :before-close="handleClose"
    :show-close="true"
    :close-on-click-modal="false"
    top="10vh"
  >
  <template #content>
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div>
        <monomer-select-area-select
          v-model:monomerId="monomerId"
          v-model:areaId="areaId"
          needConvert
          clearable
          :project-id="props.detailData.project.id"
          @change="showProjectChange"
        />
        <el-input
          v-model.trim="serialNumber"
          size="small"
          placeholder="输入编号搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="showProjectChange"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </div>
      <div>
        <print-table
          api-key="mesMonthlyTaskList"
          :params="{
            projectId: props.detailData.project.id,
            monomerId: monomerId,
            areaId: areaId,
            serialNumber: serialNumber,
          }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      :data="projectDetailData"
      return-source-data
      :max-height="maxHeight"
      highlight-current-row
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="project" key="project.shortName" label="项目" min-width="150">
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="monomerName" label="单体" align="center"></el-table-column>
      <el-table-column prop="areaName" label="区域" align="center"></el-table-column>
      <el-table-column prop="serialNumber" label="编号" align="center"></el-table-column>
      <el-table-column prop="specification" label="规格" align="center" width="150px"></el-table-column>
      <el-table-column prop="material" label="材质" align="center"></el-table-column>
      <el-table-column prop="length" label="长度" align="center"></el-table-column>
      <el-table-column prop="netWeight" label="单重" align="center"></el-table-column>
      <el-table-column prop="quantity" label="排产数" align="center"></el-table-column>
      <el-table-column prop="completeQuantity" label="完成数" align="center"></el-table-column>
      <el-table-column prop="completeMete" label="完成量" align="center"></el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { projectDetail } from '@/api/mes/task-tracking/monthly-task-tracking.js'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref } from 'vue'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { projectNameFormatter } from '@/utils/project'

const emit = defineEmits(['update:visible'])
const projectDetailData = ref([])
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showProjectDetail })

async function showProjectDetail() {
  try {
    const data = await projectDetail({
      projectId: props.detailData.project.id,
      monomerId: monomerId.value,
      areaId: areaId.value,
      serialNumber: serialNumber.value
    })
    projectDetailData.value = data
  } catch (e) {
    console.log('获取项目详情失败', e)
  }
}

// 搜索
function searchQuery() {
  showProjectDetail()
}
// 重置
function resetQuery() {
  monomerId.value = undefined
  areaId.value = undefined
  serialNumber.value = undefined
  showProjectDetail()
}

function showProjectChange() {
  showProjectDetail()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>


<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        width="200px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.project?.serialNumber }}-{{ scope.row.project?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unit" key="unit" label="单位" align="center" />
      <el-table-column prop="taskNetWeight" key="taskNetWeight" label="任务量（件/kg）" align="center">
        <template #default="{ row }">
          <span @click.stop="getTaskDetail(row)" style="cursor: pointer" class="tc-danger">
            {{ row.taskQuantity }}/{{ row.taskNetWeight }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="taskNetWeight" key="taskNetWeight" label="生产量（件/kg）" align="center">
        <template #default="{ row }">
          <span @click.stop="getProductionDetail(row)" style="cursor: pointer" class="tc-danger">
            {{ row.productionQuantity }}/{{ row.productionNetWeight }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="taskNetWeight" key="taskNetWeight" label="领料量（件/kg）" align="center">
        <template #default="{ row }">
          <span @click.stop="getPickingDetail(row)" style="cursor: pointer" class="tc-danger">
            {{ row.pickingQuantity }}/{{ row.pickingNetWeight }}
          </span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 任务量详情 -->
    <task-mete v-model:visible="drawerVisible" :info="info" />
    <!-- 生产量详情 -->
    <production-mete v-model:visible="productionDrawerVisible" :production-info="productionInfo" />
    <!-- 领料量详情 -->
    <pricking-mete v-model:visible="pickingDrawerVisible" :pricking-info="pickingInfo" />
  </div>
</template>
<script setup>
import { ref, provide } from 'vue'
import crudApi from '@/api/mes/task-tracking/wip-statistics.js'
import useCRUD from '@compos/use-crud'
import { mesSemiFinishedPM as permission } from '@/page-permission/mes'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import taskMete from './module/task-mete.vue'
import productionMete from './module/production-mete.vue'
import prickingMete from './module/picking-mete.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const drawerVisible = ref(false)
const info = ref({})
const productionDrawerVisible = ref(false)
const productionInfo = ref({})
const pickingDrawerVisible = ref(false)
const pickingInfo = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '半成品统计',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

provide('permission', permission)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 任务量详情
function getTaskDetail(row) {
  drawerVisible.value = true
  info.value = row
}

// 生产量详情
function getProductionDetail(row) {
  productionDrawerVisible.value = true
  productionInfo.value = row
}

// 领料量详情
function getPickingDetail(row) {
  pickingDrawerVisible.value = true
  pickingInfo.value = row
}
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>

<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="width: 25%">
        <div class="head-container">
          <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          highlight-current-row
          returnSourceData
          row-key="projectId"
          style="width: 100%; cursor: pointer"
          @row-click="handleProjectChange"
        >
          <el-table-column prop="index" type="index" label="序号" align="center" width="60" />
          <el-table-column
            v-if="columns.visible('projectName')"
            key="projectName"
            prop="projectName"
            :show-overflow-tooltip="true"
            label="项目"
          >
            <template v-slot="scope">
              <span>{{ scope.row.serialNumber }}-{{ scope.row.name }}</span>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <pagination />
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div>
      <project-process-detail :process-data="processData" style="flex: 1" />
    </div>
  </div>
</template>
<script setup>
import { ref, watch } from 'vue'
import crudApi from '@/api/bridge/production-manage/dashboard/project-overview'
import { bridgeProjectOverviewPM as permission } from '@/page-permission/bridge'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import projectProcessDetail from './project-process-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const processData = ref({})
const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '项目总览',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

watch(
  () => crud.query.year,
  (val) => {
    processData.value = {}
  }
)
watch(
  () => crud.query.status,
  (val) => {
    processData.value = {}
  }
)
watch(
  () => crud.query.name,
  (val) => {
    processData.value = {}
  }
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleProjectChange(row) {
  processData.value = row
}

CRUD.HOOK.handleRefresh = (crud, res) => {}
</script>
<style lang="scss" scoped>
</style>

<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="width: 30%;">
        <div class="head-container">
            <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="plateList"
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
            align="center"
            key="projectName"
            prop="projectName"
            :show-overflow-tooltip="true"
            label="项目"
          >
            <template v-slot="scope">
              <span>{{ scope.row.projectName }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div>
      <project-process-detail :process-data="processData" style="flex: 1" />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
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
const { crud, columns } = useCRUD(
  {
    title: '项目总览',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleProjectChange(row) {
  processData.value = row
}

// CRUD.HOOK.handleRefresh = (crud, res) => {
//   res.data.content = res.data.content.map((v) => {
//     return v
//   })
// }
</script>
<style lang="scss" scoped>
</style>

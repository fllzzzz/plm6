<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="tableData"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        highlight-current-row
        row-key="projectId"
        style="width: 100%; flex: 1"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="columns.visible('projectName')"
          align="center"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="项目"
        >
          <template v-slot="scope">
            <span>{{ scope.row.projectNumber }}-{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="nestingStatusEnum" prop="nestingStatusEnum" :show-overflow-tooltip="true" label="状态">
          <template v-slot="scope">
            <el-tag style="width: 80px" :type="projectNestingStatusEnum.V[scope.row.nestingStatusEnum].T">{{ projectNestingStatusEnum.VL[scope.row.nestingStatusEnum] }}</el-tag>
          </template>
        </el-table-column>
      </common-table>
       <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <project-detail style="flex: 3" />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { projectNestingStatusEnum } from '@enum-ms/mes'
import mHeader from './module/header.vue'
import projectDetail from './project-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '型材套料',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const tableData = [
  { projectNumber: 1111, projectName: '项目', nestingStatusEnum: 1 },
  { projectNumber: 2222, projectName: '项目', nestingStatusEnum: 2 },
  { projectNumber: 3333, projectName: '项目', nestingStatusEnum: 4 }
]
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>

<style lang="scss" scoped>
</style>

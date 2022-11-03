<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        highlight-current-row
        row-key="projectId"
        style="width: 30%;"
        @row-click="handleRowChange"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="columns.visible('projectName')"
          key="projectName"
          prop="projectName"
          :show-overflow-tooltip="true"
          label="项目"
        >
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="nestingStatusEnum" prop="nestingStatusEnum" :show-overflow-tooltip="true" label="状态">
          <template v-slot="scope">
            <el-tag style="width: 80px" :type="projectNestingStatusEnum.V[scope.row.nestingStatusEnum].T">{{ projectNestingStatusEnum.VL[scope.row.nestingStatusEnum] }}</el-tag>
          </template>
        </el-table-column>
      </common-table>
       <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <project-detail :project-data="projectData" style="flex: 1" />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/craft-manage/section-steel/nesting'
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
const projectData = ref({})
const { crud, columns } = useCRUD(
  {
    title: '型材套料',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
function handleRowChange(row) {
  console.log(row,'row');
  projectData.value = row
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>

<style lang="scss" scoped>
</style>

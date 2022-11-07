<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="min-width: 25%">
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
          row-key="projectId"
          style="width: 100%"
          @row-click="handleChangeProject"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column
            v-if="columns.visible('projectName')"
            header-align="center"
            key="projectName"
            prop="projectName"
            :show-overflow-tooltip="true"
            label="项目"
            min-width="160px"
          >
            <template #default="{ row }">
              <span>{{ row.projectName }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="nestingStatusEnum" prop="nestingStatusEnum" :show-overflow-tooltip="true" label="状态">
            <template #default="{ row }">
              <el-tag style="width: 80px" :type="projectNestingStatusEnum.V[row.nestingStatusEnum].T">{{
                projectNestingStatusEnum.VL[row.nestingStatusEnum]
              }}</el-tag>
            </template>
          </el-table-column>
        </common-table>
        <pagination />
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 130px)"></div>
      <nesting-result-detail :batch-row="batchRow" @success="crud.toQuery" style="flex: 1" />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/craft-manage/section-steel/nesting-result'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { projectNestingStatusEnum } from '@enum-ms/mes'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import nestingResultDetail from './nesting-result-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const batchRow = ref({})
const { crud, columns } = useCRUD(
  {
    title: '套料成果',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function handleChangeProject(row) {
  batchRow.value = row
}
</script>

<style lang="scss" scoped>
</style>

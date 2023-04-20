<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container project-head-container" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      highlight-current-row
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      @current-change="currentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column key="project" prop="project" label="项目列表" min-width="100" align="center" />
      <el-table-column key="status" prop="status" label="状态" width="92" align="center">
        <template #default="{ row }">
          <el-tag size="medium" :type="productionKanbanTypeEnum.V?.[row.sourceRow.status]?.TAG">
            {{ row.status }}
          </el-tag>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { projectList as get } from '@/api/enclosure/production-manage/task-tracking'
import { ref, defineEmits } from 'vue'

import { enclosureSchedulingManagePM as permission } from '@/page-permission/enclosure'
import { productionKanbanTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const emit = defineEmits(['project-change'])
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const project = ref({})
const dataFormat = ref([
  ['project', 'parse-project'],
  ['status', ['parse-enum', productionKanbanTypeEnum]]
])

const { crud, CRUD } = useCRUD(
  {
    title: '排产管理-项目列表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: '.project-head-container'
})

function currentChange(row) {
  project.value = row?.project || {}
  emit('project-change', project.value)
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = data
}
</script>


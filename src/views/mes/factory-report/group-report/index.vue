<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <process-list :maxHeight="maxHeight - 40" @nesting-task-click="handleNestingTaskClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.processId" type="info" size="medium"> * 请点击左侧项目列表查看详情 </el-tag>
      <template v-else>
        <div class="head-container">
          <!-- <mHeader /> -->
        </div>
        <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
          <div>
            <el-tag size="medium">车间：{{ info?.workshop?.name }}>{{ info?.groups?.name }}</el-tag>
            <el-tag size="medium" style="margin-left: 10px">工序：{{ info?.process?.name }}</el-tag>
          </div>
          <div style="width: 300px">
            <print-table
              :api-key="apiKey"
              :params="{
                ...query,
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
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :dataFormat="dataFormat"
          :max-height="maxHeight"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="70" />
          <el-table-column label="项目" type="project" align="center" min-width="120">
            <template #default="{ row }">
              <span>{{ row.project?.serialNumber }}-{{ row.project?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="area.name" label="单体" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单重（kg）" align="center" />
        </common-table>
        <!--分页组件-->
        <pagination />
      </template>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/factory-report/group-report.js'
import { ref, provide } from 'vue'

import { artifactWorkOrderPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
// import mHeader from './module/header'
// import detail from './module/detail.vue'
import processList from './module/process-list.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const info = ref({})
const { crud, CRUD } = useCRUD(
  {
    title: '班组报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['processId']
  },
  tableRef
)

const dataFormat = ref([['scheduleTime', ['parse-time', '{y}-{m}-{d}']]])
provide('crud', crud)
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

function handleNestingTaskClick(val, query) {
  crud.query.processId = val?.process?.id
  crud.query.groupsId = val?.groups?.id
  crud.query.teamId = val?.team?.id
  crud.query.taskTypeEnum = val?.taskTypeEnum
  info.value = val
  //   if (crud.query.processId) {
  crud.toQuery()
  //   }
}
</script>
<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 500px;
    margin-right: 20px;
    overflow-x: auto;
  }
  .wrap-right {
    flex: 31;
    min-width: 0;
    overflow: hidden;
  }
}
</style>

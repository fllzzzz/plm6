<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        key="productionLine.name"
        prop="productionLine.name"
        :show-overflow-tooltip="true"
        label="生产线"
        min-width="180px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLine.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskLength')"
        key="taskLength"
        prop="taskLength"
        :show-overflow-tooltip="true"
        label="任务量(m)"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.taskLength, DP.MES_ENCLOSURE_L__M) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeLength')"
        key="completeLength"
        prop="completeLength"
        :show-overflow-tooltip="true"
        label="完成量(m)"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.completeLength, DP.MES_ENCLOSURE_L__M) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeRate')"
        key="completeRate"
        prop="completeRate"
        :show-overflow-tooltip="true"
        label="完成状态"
        min-width="250px"
      >
        <template v-slot="scope">
          <el-progress :text-inside="true" :stroke-width="20" :percentage="scope.row.completeRate" :color="colors" />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail v-model:visible="detailVisible" :info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/enclosure-team'
import { ref, reactive } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

// const customColor = '#909399'
const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护班组',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.completeRate = Number(toFixed((v.completeLength / v.taskLength) * 100, 2))
    return v
  })
}

let detailInfo = reactive({})
const detailVisible = ref(false)
function showDetail(row) {
  detailVisible.value = true
  detailInfo = Object.assign(detailInfo, row)
}
</script>

<style lang="scss" scoped></style>
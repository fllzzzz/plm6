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
      row-key="rowId"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('userName')" :show-overflow-tooltip="true" align="center" prop="userName" label="检验人">
        <template #default="{ row }">
          <span>{{ row.userName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inspectionQuantity')"
        :show-overflow-tooltip="true"
        align="center"
        prop="inspectionQuantity"
        label="检验数"
      >
        <template #default="{ row }">
          <span>{{ row.inspectionQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('qualifiedQuantity')"
        :show-overflow-tooltip="true"
        align="center"
        prop="qualifiedQuantity"
        label="合格数"
      >
        <template #default="{ row }">
          <span class="tc-success">{{ row.qualifiedQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('qhseQuantity')"
        :show-overflow-tooltip="true"
        align="center"
        prop="qhseQuantity"
        label="不合格数"
      >
        <template #default="{ row }">
          <span class="tc-danger">{{ row.qhseQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" v-permission="permission.detail" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button size="mini" type="primary" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-detail v-model:visible="detailVisible" :info="itemInfo" :projectId="crud.query.projectId" ></m-detail>
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/QHSE-manage/quality-inspection-report'
import { provide, ref } from 'vue'

import { bridgeQualityInspectionReportPM as permission } from '@/page-permission/bridge'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '质检报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

provide('query', crud.query)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.qualifiedQuantity = v.inspectionQuantity - v.qhseQuantity
    return v
  })
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  detailVisible.value = true
  itemInfo.value = row
}
</script>

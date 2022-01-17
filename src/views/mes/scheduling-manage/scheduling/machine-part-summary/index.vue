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
      row-key="rowId"
      @selection-change="crud.selectionChangeHandler"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120px"
        align="center"
      >
        <template #default="{ row }">
          <span v-empty-text="{ val: row.name, blank: '未分类' }"></span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('steelSpec')"
        prop="steelSpec"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板厚/规格"
        align="center"
      >
        <template #default="{ row }">
          <span v-empty-text="{ val: row.steelSpec, blank: '未分类' }"></span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        prop="quantity"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="数量"
        align="center"
      >
        <template #default="{ row }">
          <span v-empty-text="row.quantity"></span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        prop="totalNetWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="重量"
        align="center"
      >
        <template #default="{ row }">
          <span v-to-fixed="{ k: 'COM_WT__KG', val: row.totalNetWeight }" v-empty-text></span>
        </template>
      </el-table-column>
      <el-table-column label="详情" align="center" width="130">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 详情 -->
    <common-drawer
      v-model:visible="drawerVisible"
      :title="`零件明细表：${detailRow?.name} - ${detailRow?.steelSpec}`"
      direction="rtl"
      size="100%"
      :before-close="
        () => {
          drawerVisible = false
        }
      "
    >
      <template #content>
        <m-detail :fQuery="fQuery" @refresh="crud.toQuery" :visible="drawerVisible" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling/machine-part-summary'
import { provide, ref } from 'vue'

import { machinePartSchedulingPM as permission } from '@/page-permission/mes'
import { componentTypeEnum } from '@enum-ms/mes'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from '../machine-part/index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productType = componentTypeEnum.MACHINE_PART.V
provide('productType', productType)

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '零件排产',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaIds'],
    invisibleColumns: [],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v, i) => {
    v.rowId = i + '' + Math.random()
  })
}

const detailRow = ref({})
const fQuery = ref({})
const drawerVisible = ref(false)

function showDetail(row) {
  detailRow.value = row
  fQuery.value = { ...deepClone(crud.query), ...deepClone(row) }
  drawerVisible.value = true
}
</script>

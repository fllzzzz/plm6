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
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="rowId"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        prop="name"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="100px"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('steelSpec')"
        prop="steelSpec"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板厚/规格"
        min-width="100px"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.steelSpec }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        :show-overflow-tooltip="true"
        prop="quantity"
        :label="`清单量`"
        align="center"
        min-width="140px"
      >
        <template #default="{ row }">
          <span class="quantity-mete-show">
            <span class="left">{{ row.quantity }} 件</span>
            <span class="line">|</span>
            <span class="right">
              <span>{{ row.totalNetWeight }}</span> kg
            </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalSchedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="totalSchedulingQuantity"
        :label="`已分配`"
        align="center"
        min-width="140px"
      >
        <template #default="{ row }">
          <span class="quantity-mete-show tc-success">
            <span class="left">{{ row.totalSchedulingQuantity }} 件</span>
            <span class="line">|</span>
            <span class="right">
              <span>{{ row.totalSchedulingMete }}</span> kg
            </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unSchedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="unSchedulingQuantity"
        :label="`未分配`"
        align="center"
        min-width="140px"
      >
        <template #default="{ row }">
          <span class="quantity-mete-show tc-danger">
            <span class="left">{{ row.unSchedulingQuantity }} 件</span>
            <span class="line">|</span>
            <span class="right">
              <span>{{ row.unSchedulingMete }}</span> kg
            </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column label="操作" align="center" width="130">
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
      :title="`零件明细表：${detailRow?.name || '未分类'} - ${detailRow?.steelSpec || '未分类'}`"
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

const dataFormat = ref([
  ['name', ['empty-text', '未分类']],
  ['steelSpec', ['empty-text', '未分类']],
  ['totalNetWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['totalSchedulingMete', ['to-fixed-ck', 'COM_WT__KG']],
  ['unSchedulingMete', ['to-fixed-ck', 'COM_WT__KG']]
])

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
    v.totalSchedulingMete = v.totalSchedulingNetWeight || 0
    v.unSchedulingQuantity = v.quantity - v.totalSchedulingQuantity || 0
    v.unSchedulingMete = v.totalNetWeight - v.totalSchedulingMete || 0
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

<style lang="scss">
.quantity-mete-show {
  display: flex;

  .left {
    width: 50%;
    text-align: right;
  }

  .right {
    width: 50%;
    text-align: left;
  }

  .line {
    width: 15px;
    text-align: center;
  }
}
</style>

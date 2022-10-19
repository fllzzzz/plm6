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
      :dataFormat="dataFormat"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('createTime')"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="排产日期"
        width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('orderNumber')"
        :show-overflow-tooltip="true"
        prop="orderNumber"
        label="排产单号"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('userName')"
        :show-overflow-tooltip="true"
        prop="userName"
        label="排产人"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('productType')"
        :show-overflow-tooltip="true"
        prop="productType"
        label="类型"
        width="80px"
        align="center"
      >
        <template #default="{ row }">
          <el-tag :type="componentTypeEnum.V[row.productType].T" effect="plain">{{ componentTypeEnum.VL[row.productType] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('factory.name')"
        :show-overflow-tooltip="true"
        prop="factory.name"
        label="车间"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        :show-overflow-tooltip="true"
        prop="productionLine.name"
        label="生产线"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        :show-overflow-tooltip="true"
        prop="quantity"
        label="任务数（件）"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        :show-overflow-tooltip="true"
        prop="totalNetWeight"
        label="任务量（kg）"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('status')"
        :show-overflow-tooltip="true"
        prop="status"
        label="状态"
        min-width="110px"
        align="center"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/work-order-manage/artifact.js'
import { ref } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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
    title: '结构工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: []
  },
  tableRef
)

const dataFormat = ref([['createTime', ['parse-time', '{y}-{m}-{d}']]])

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>

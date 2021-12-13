<template>
  <div>
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
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('plate')"
        key="plate"
        prop="plate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板型"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`板厚\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        sortable="custom"
        :label="`总面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总长度\n(m)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalLength, DP.MES_ENCLOSURE_L__M) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        label="数量"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="【工序 │ 数量】"
        min-width="160px"
      >
        <template v-slot="scope">
          <span v-html="scope.row.processSequence" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('shipQuantity')"
        key="shipQuantity"
        prop="shipQuantity"
        sortable="custom"
        label="发运"
        align="center"
        min-width="70px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { artifact as get } from '@/api/mes/production-manage/dashboard/project-report'
import { ref, defineExpose, inject } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

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
    title: '项目报表-围护',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    // v.processSequence = v.process
    //   .map((v) => {
    //     return `<span>【${v.processName} │ <span style="color: #67C23A;">${v.quantity}</span>】</span>`
    //   })
    //   .join('<span>→</span>')
    return v
  })
}

const commonQuery = inject('commonQuery')
CRUD.HOOK.beforeToQuery = (crud, res) => {
  crud.query = Object.assign(crud.query, commonQuery)
}

defineExpose({
  toQuery: crud.toQuery
})
</script>

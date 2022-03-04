<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="headRef" />
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
      <productType-summary-columns :productType="crud.query.productType" :columns="columns" :unitNewLine="false" />
      <el-table-column align="center" prop="prop" label="操作" width="100">
        <template #default="{ row }">
          <!-- <common-button type="primary" size="mini">全部修改</common-button> -->
          <common-button type="warning" size="mini" @click="handleSeveralEdit(row)">修改</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/wages-adjust/summary'
import detailApi from '@/api/mes/team-report/wages-adjust/detail'
import { ref, provide, defineExpose, defineEmits, inject } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import productTypeSummaryColumns from '@comp-mes/table-columns/productType-summary-columns'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const emit = defineEmits(['setInfo', 'setDetailInfo', 'refreshAuditNumber'])

const permission = inject('permission')
const headRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工价调整汇总',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: true })
provide('query', crud.query)

CRUD.HOOK.beforeRefresh = () => {
  crud.crudApi.get = crud.query.productType & componentTypeEnum.ASSEMBLE.V ? detailApi.get : crudApi.get
  emit('refreshAuditNumber')
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    return v
  })
}

function handleSeveralEdit(row) {
  if (crud.query.productType & componentTypeEnum.ASSEMBLE.V) {
    emit('setDetailInfo', row)
  } else {
    emit('setInfo', row)
  }
}

defineExpose({
  query: crud.query
})
</script>

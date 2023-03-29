<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      return-source-data
      :data-format="dataFormat"
      style="width: 100%"
      class="businessTable"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        show-overflow-tooltip
        label="名称"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        show-overflow-tooltip
        label="规格"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        show-overflow-tooltip
        label="材质"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('totalQuantity')"
        key="totalQuantity"
        prop="totalQuantity"
        label="数量"
        align="center"
        min-width="70"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        label="总长度(米)"
        align="center"
        min-width="70"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('totalWeight')"
        key="totalWeight"
        prop="totalWeight"
        show-overflow-tooltip
        label="总量(t)"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('totalPrice')"
        key="totalPrice"
        prop="totalPrice"
        align="center"
        min-width="120"
        label="金额"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/structure'
import { ref } from 'vue'
import { transactionRecordPM as permission } from '@/page-permission/contract'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['unitPrice', 'to-thousand'],
  ['totalPrice', 'to-thousand']
])

const { crud, columns } = useCRUD(
  {
    title: '结构制品',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['projectId']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 100
})
</script>

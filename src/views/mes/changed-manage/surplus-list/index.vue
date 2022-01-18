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
      <belonging-info-columns :columns="columns" showFactory showWorkshop showProductionLine showProcess showTeam />
      <productType-base-info-columns :productType="productType" :columns="columns" :unShowField="['specification', 'material', 'name']" />
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="多余数量"
        align="center"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" :show-overflow-tooltip="true" prop="status" label="状态" align="center" width="100">
        <template #default="{ row }">
          <el-tag :type="surplusHandleStatusEnum.V[row.status].TAG">{{ surplusHandleStatusEnum.VL[row.status] }}</el-tag>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/changed-manage/surplus-list'
import { ref, computed } from 'vue'

import { surplusListPM as permission } from '@/page-permission/mes'
import { surplusHandleStatusEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '多余列表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const productType = computed(() => {
  return crud.query.productType
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>

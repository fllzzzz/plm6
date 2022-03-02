<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      class="businessTable"
      :max-height="maxHeight"
      :cell-class-name="changedCellMask"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" show-overflow-tooltip label="名称" align="center" min-width="120">
        <template #default="{ row }">
          <span v-empty-text="row.name" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('material')" key="material" prop="material" show-overflow-tooltip label="材质" align="center" min-width="120">
        <template #default="{ row }">
          <span v-empty-text="row.material" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalQuantity')" key="totalQuantity" prop="totalQuantity" label="数量" align="center" min-width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text="row.totalQuantity" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalWeight')" key="totalWeight" prop="totalWeight" label="总量(t)" align="center" min-width="120">
        <template #default="{ row }">
          <span v-empty-text="row.totalWeight" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unitPrice')" key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="综合单价" align="center" min-width="120">
        <template #default="{ row }">
          <common-input-number
            v-if="headerRef && headerRef.modifying"
            v-model="row.unitPrice"
            :step="1"
            :min="0"
            :max="99999999"
            :precision="DP.YUAN"
            size="small"
            style="width: 100%"
            @change="handlePrice(row)"
          />
          <template v-else>
          <span v-thousand="row.unitPrice" v-empty-text />
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" align="center" min-width="120" label="金额">
        <template #default="{ row }">
          <span v-thousand="row.totalPrice" v-empty-text />
        </template>
      </el-table-column>
      <!--详情-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button icon="el-icon-view" type="info" size="mini" @click.stop="openDetail(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail :detail-info="detailInfo" @refresh="crud.refresh" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/structure'
import { ref, defineExpose } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'

import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'

import useTableChange from '@compos/form/use-table-change'
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

const sourceMap = new Map([
  ['unitPrice', 'originUnitPrice']
])

const tableRef = ref()
const headerRef = ref()
const detailInfo = ref({})
const { crud, columns } = useCRUD(
  {
    title: '结构价格',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    requiredQuery: ['monomerId']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 查看构件详情
function openDetail(row) {
  detailInfo.value = row
  crud.toDetail(row)
}

// 价格变动
function handlePrice(row) {
  row.totalPrice = row.totalWeight * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>

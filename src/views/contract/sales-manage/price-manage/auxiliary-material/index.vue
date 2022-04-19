<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" v-bind="$attrs" />
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
      :cell-class-name="changedCellMask"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" show-overflow-tooltip label="名称" align="center" min-width="120">
        <template #default="{ row }">
          <el-tooltip :content="row.classifyFullName" :disabled="!row.classifyFullName" :show-after="200" placement="top">
            <span>{{ row.classifyName }}</span>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" show-overflow-tooltip label="编号" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('specification')" key="specification" prop="specification" show-overflow-tooltip label="规格" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('color')" key="color" prop="color" show-overflow-tooltip label="颜色" align="center" width="100" />
      <el-table-column v-if="columns.visible('accountingUnit')" key="accountingUnit" prop="accountingUnit" show-overflow-tooltip label="核算单位" align="center" width="100" />
      <el-table-column v-if="columns.visible('mete')" key="mete" prop="mete" show-overflow-tooltip label="核算量" align="center" min-width="100" />
      <el-table-column v-if="columns.visible('unitPrice')" key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="综合单价" align="center" min-width="120">
        <template #default="{ row }">
          <common-input-number
            v-if="headerRef && headerRef.modifying"
            v-model="row.newUnitPrice"
            :step="1"
            :min="0"
            :max="99999999"
            :precision="DP.YUAN"
            size="small"
            style="width: 100%"
            @change="handlePrice(row)"
          />
          <template v-else>
            <span>{{ row.unitPrice }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" align="center" min-width="120" label="金额" />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/auxiliary-material'
import { ref, defineExpose } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'

import { DP } from '@/settings/config'

import useTableChange from '@compos/form/use-table-change'
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

const sourceMap = new Map([
  ['unitPrice', 'originUnitPrice']
])

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['unitPrice', 'to-thousand'],
  ['totalPrice', 'to-thousand']
])
const { crud, columns } = useCRUD(
  {
    title: '配套件价格',
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

// 价格变动
function handlePrice(row) {
  row.unitPrice = row.newUnitPrice
  row.totalPrice = row.mete * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>

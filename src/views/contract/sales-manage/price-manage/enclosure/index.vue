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
      <el-table-column v-if="columns.visible('plate')" key="plate" prop="plate" show-overflow-tooltip label="板型" align="center" width="100">
        <template #default="{ row }">
          <span v-empty-text="row.plate" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('thickness')" key="thickness" prop="thickness" show-overflow-tooltip label="厚度(mm)" align="center">
        <template #default="{ row }">
            <span v-empty-text>{{ toFixed(row.thickness, DP.MES_ENCLOSURE_T__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('color')" key="color" prop="color" show-overflow-tooltip label="颜色" align="center" width="100">
        <template #default="{ row }">
          <span v-empty-text="row.color" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalQuantity')" key="totalQuantity" prop="totalQuantity" :show-overflow-tooltip="true" label="数量(张)" align="center" width="100" />
      <el-table-column v-if="columns.visible('totalArea')" key="totalArea" prop="totalArea" show-overflow-tooltip label="总面积(㎡)" align="center">
        <template #default="{ row }">
          <span v-empty-text="row.totalArea" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalLength')" key="totalLength" prop="totalLength" show-overflow-tooltip label="总长度(m)" align="center">
        <template #default="{ row }">
          <span v-empty-text="row.totalLength" />
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
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/price-manage/enclosure'
import { ref, defineExpose, inject } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
import { enclosureSettlementTypeEnum } from '@enum-ms/contract'

import useTableChange from '@compos/form/use-table-change'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

// 围护结算类型
const enclosureMeasureMode = inject('enclosureMeasureMode')

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
const { crud, columns } = useCRUD(
  {
    title: '围护价格',
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
  row.totalPrice = (enclosureMeasureMode.value === enclosureSettlementTypeEnum.LENGTH.V ? row.totalLength : row.totalArea) * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>

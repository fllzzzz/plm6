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
      <el-table-column v-if="columns.visible('useProperty')" prop="useProperty" label="使用类别" align="center">
        <template #default="{ row }">
          <span>{{ row.useProperty?auxiliaryMaterialUseTypeEnum.VL[row.useProperty]:'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        prop="name"
        label="名称"
        align="center"
        v-if="columns.visible('name')"
        :show-overflow-tooltip="true"
      />
      <el-table-column v-if="columns.visible('specification')" :show-overflow-tooltip="true" prop="specification" label="规格" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('measureUnit')" :show-overflow-tooltip="true" prop="measureUnit" label="单位" align="center"/>
      <el-table-column v-if="columns.visible('quantity')" :show-overflow-tooltip="true" prop="quantity" label="数量" align="center"/>
      <el-table-column
        v-if="columns.visible('unitPrice')"
        key="unitPrice"
        prop="unitPrice"
        :show-overflow-tooltip="true"
        label="综合单价"
        align="center"
        min-width="120"
      >
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
            <span :class="row.status === 1 ? 'tc-danger' : ''">{{ row.unitPrice }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" key="totalPrice" prop="totalPrice" align="center" min-width="120" label="金额">
        <template #default="{ row }">
          <span :class="row.status === 1 ? 'tc-danger' : ''">{{ row.totalPrice }}</span>
        </template>
      </el-table-column>
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
import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'

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

const sourceMap = new Map([['unitPrice', 'originUnitPrice']])

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['unitPrice', ['to-thousand-ck', 'YUAN']],
  ['totalPrice', ['to-thousand-ck', 'YUAN']]
])
const { crud, columns } = useCRUD(
  {
    title: '配套件价格',
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
const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 价格变动
function handlePrice(row) {
  row.unitPrice = row.newUnitPrice
  row.totalPrice = row.quantity * (row.unitPrice || 0)
}

defineExpose({
  refresh: crud.refresh
})
</script>

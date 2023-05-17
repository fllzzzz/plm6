<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" align="left" label="客户名称" min-width="160" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="订单类型" align="center" min-width="100" :show-overflow-tooltip="true" />
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" label="订单数量" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('totalContractAmount')" prop="totalContractAmount" key="totalContractAmount" label="累计合同额" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('totalSettlementAmount')" prop="totalSettlementAmount" key="totalSettlementAmount" label="累计结算额" align="center" min-width="120" show-overflow-tooltip />
      <!--详情-->
      <el-table-column
        v-if="checkPermission(permission.detail)"
        label="操作"
        width="80px"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <common-button size="mini" type="info" icon="el-icon-view" @click="showDetail(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail v-model:visible="detailVisible" :info="itemInfo" :permission="permission"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/transaction-record'
import { ref } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { transactionRecordPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'

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

const tableRef = ref()
const dataFormat = ref([
  ['businessType', ['parse-enum', businessTypeEnum]],
  ['totalContractAmount', ['to-thousand-ck', 'YUAN']],
  ['totalSettlementAmount', ['to-thousand-ck', 'YUAN']]
])
const { crud, columns } = useCRUD(
  {
    title: '客户交易记录',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  itemInfo.value = Object.assign({ ...crud.query }, row.sourceRow)
  detailVisible.value = true
}
</script>

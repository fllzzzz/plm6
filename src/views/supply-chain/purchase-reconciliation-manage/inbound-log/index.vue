<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
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
      <el-table-column v-if="columns.visible('createTime')" show-overflow-tooltip key="createTime" prop="createTime" label="日期" align="center" width="130" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购订单" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('supplierName')" show-overflow-tooltip key="supplierName" prop="supplierName" label="供应商"/>
      <el-table-column v-if="columns.visible('typeText')" show-overflow-tooltip key="typeText" prop="typeText" label="物料种类" />
      <el-table-column v-if="columns.visible('mete')" show-overflow-tooltip align="center" key="mete" prop="mete" label="合同量">
        <template #default="{ row }">
          <span>{{ row.mete }}{{row.meteUnit}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="center" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('inboundAmount')" prop="inboundAmount" key="inboundAmount" label="入库额" align="center" show-overflow-tooltip />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission(permission.detail)"
        label="操作"
        width="190px"
        align="center"
      >
        <template #default="{ row }">
          <common-button type="success" icon="el-icon-view" size="mini" @click="openRecord(row)" v-if="checkPermission(permission.detail)"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination/>
    <!-- 记录 -->
    <inboundRecord v-model="recordVisible" :detail-info="detailInfo" :permission="permission"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, provide, nextTick } from 'vue'

import { supplierMaterialInboundPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'
import { matClsEnum } from '@/utils/enum/modules/classification'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import inboundRecord from '@/views/supply-chain/purchase-reconciliation-manage/payment-ledger/module/inbound-record'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand'],
  ['inboundRate', ['to-fixed', 2]]
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '入库记录',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const orderId = ref(undefined)
const recordVisible = ref(false)

provide('orderId', orderId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.typeText = basicClassArr.join(' | ')
  })
}

// 打开记录
function openRecord(row, type) {
  detailInfo.value = row.sourceRow
  orderId.value = row.id
  nextTick(() => {
    recordVisible.value = true
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
}
</style>

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
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目" min-width="250" />
      <el-table-column v-if="columns.visible('contractAmount')" prop="contractAmount" key="contractAmount" label="合同额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="结算额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="申请日期" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" align="center" label="申请人" min-width="110" />
      <el-table-column v-if="columns.visible('checkUserName')" key="checkUserName" prop="checkUserName" :show-overflow-tooltip="true" align="center" label="审核人" min-width="110" />
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" :show-overflow-tooltip="true" align="center" label="状态" min-width="110">
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row?.sourceRow?.status].TAG" size="medium" effect="plain">{{ row.status }}</el-tag>
        </template>
      </el-table-column>
      <!--详情与审核-->
      <el-table-column v-permission="[...permission.detail, ...permission.edit, ...permission.audit]" label="操作" width="140" fixed="right">
        <template #default="{ row }">
          <common-button v-if="row?.sourceRow?.status === reviewStatusEnum.PASS.V" v-permission="permission.detail" type="primary" size="mini" @click.stop="toDetail(row)">查看</common-button>
          <common-button v-else v-permission="permission.edit" type="primary" size="mini" @click.stop="crud.toEdit(row)">编辑</common-button>
          <common-button v-if="row?.sourceRow?.status === reviewStatusEnum.UNREVIEWED.V" v-permission="permission.audit" type="success" size="mini" @click.stop="toDetail(row)">确签</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail  :status="visaStatus" @success="crud.toQuery()" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/settlement-manage'
import { ref, computed } from 'vue'

import { settlementManagePM as permission } from '@/page-permission/contract'
import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const visaStatus = ref()
const tableRef = ref()

const dataFormat = computed(() => {
  return [
    ['project', 'parse-project'],
    ['status', ['parse-enum', reviewStatusEnum, { f: 'SL' }]],
    ['contractAmount', ['to-thousand', decimalPrecision.value.contract]],
    ['amount', ['to-thousand', decimalPrecision.value.contract]],
    ['createTime', 'parse-time']
  ]
})
const { crud, columns } = useCRUD(
  {
    title: '结算管理',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    invisibleColumns: [],
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 打开详情
function toDetail(row) {
  visaStatus.value = row?.sourceRow?.status
  crud.toDetail(row)
}
</script>

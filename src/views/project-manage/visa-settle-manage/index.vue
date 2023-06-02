<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader @to-add="toAdd" />
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
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目" min-width="250">
        <template #default="{ row }">
          <table-cell-tag :show="row.isSubmitSettle === settlementStatusEnum.SETTLED.V && row.type === visaTypeEnum.SETTLEMENT.V" :name="settlementStatusEnum.SETTLED.L" color="#f56c6c"/>
          <span style="margin-left: 14px">{{ row.project }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('reasonName')" key="reasonName" prop="reasonName" :show-overflow-tooltip="true" align="center" label="属性" min-width="110">
        <template #default="{ row }">
          <span @click.stop="toDetail(row)" style="cursor:pointer;">{{ row.reasonName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="签证(结算)额" align="center" min-width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <span @click.stop="toDetail(row)" style="cursor:pointer;">{{ row.amount }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="申请日期" align="center" min-width="120" />
      <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" align="center" label="申请人" min-width="110" />
      <el-table-column v-if="columns.visible('checkUserName')" key="checkUserName" prop="checkUserName" :show-overflow-tooltip="true" align="center" label="审核人" min-width="110" />
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" :show-overflow-tooltip="true" align="center" label="状态" min-width="110">
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row?.sourceRow?.status].TAG" size="medium" effect="plain">{{ row.status }}</el-tag>
        </template>
      </el-table-column>
      <!--详情与审核-->
      <el-table-column v-if="checkPermission([...permission.detail, ...permission.edit, ...permission.audit])" label="操作" width="150px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button v-if="row?.sourceRow?.status === reviewStatusEnum.PASS.V" v-permission="permission.detail" type="primary" size="mini" @click.stop="toDetail(row,'detail')">查看</common-button>
          <common-button v-else v-permission="permission.edit" type="primary" size="mini" @click.stop="toEdit(row)">编辑</common-button>
          <common-button v-if="row?.sourceRow?.status === reviewStatusEnum.UNREVIEWED.V" v-permission="permission.audit" type="success" size="mini" @click.stop="toDetail(row,'audit')">确签</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <component :is="mForm" />
    <component :is="mDetail" :status="visaStatus" @success="crud.toQuery()" :showType="showType"/>
  </div>
</template>

<script setup>
import crudApi, { addVisa, addSettlement, editVisa, editSettlement } from '@/api/project-manage/visa-settle-manage'
import { ref, computed } from 'vue'

import { visaSettlePM as permission } from '@/page-permission/project'
import { reviewStatusEnum, visaTypeEnum } from '@enum-ms/common'
import { settlementStatusEnum } from '@enum-ms/finance'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import visaForm from './visa/form'
import visaDetail from './visa/detail.vue'
import settlementForm from './settlement/form'
import settlementDetail from './settlement/detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const visaStatus = ref()
const visaType = ref(visaTypeEnum.VISA.V)
const tableRef = ref()
const showType = ref('detail')

// 新增
const mForm = computed(() => {
  return visaType.value === visaTypeEnum.VISA.V ? visaForm : settlementForm
})

// 详情
const mDetail = computed(() => {
  return visaType.value === visaTypeEnum.VISA.V ? visaDetail : settlementDetail
})

const dataFormat = ref([
  ['project', ['parse-project', { onlyShortName: true }]],
  ['status', ['parse-enum', reviewStatusEnum, { f: 'SL' }]],
  ['amount', ['to-thousand-ck', 'YUAN']],
  ['createTime', 'parse-time']
])
const { crud, columns } = useCRUD(
  {
    title: '签证结算',
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
function toDetail(row, type) {
  if (!checkPermission(permission.detail)) {
    return
  }
  showType.value = type
  visaType.value = row.type
  visaStatus.value = row?.sourceRow?.status
  crud.crudApi.add = visaType.value === visaTypeEnum.VISA.V ? addVisa : addSettlement
  crud.toDetail(row)
}

// 新增表单
function toAdd(type) {
  visaType.value = type
  crud.crudApi.add = visaType.value === visaTypeEnum.VISA.V ? addVisa : addSettlement
  crud.toAdd()
}

// 编辑表单
function toEdit(row) {
  visaType.value = row.type
  crud.crudApi.edit = visaType.value === visaTypeEnum.VISA.V ? editVisa : editSettlement
  crud.toEdit(row.sourceRow)
}
</script>

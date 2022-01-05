<template>
  <div class="app-container" style="padding: 0">
    <!--工具栏-->
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
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" :show-overflow-tooltip="true" label="变更类型" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.type ? contractChangeTypeEnum.VL[scope.row.type] : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('amount')"
        key="amount"
        prop="amount"
        :show-overflow-tooltip="true"
        label="金额(元)"
        min-width="250"
      >
        <template v-slot="scope">
          <span>{{ scope.row.amount ? toThousand(scope.row.amount) : scope.row.amount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('contractAmount')"
        key="contractAmount"
        prop="contractAmount"
        label="合同金额(元)"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.contractAmount ? toThousand(scope.row.contractAmount) : scope.row.contractAmount }}</div>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('leaderList')"
        key="leaderList"
        prop="leaderList"
        label="负责人"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.leaderList }}</div>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('changeDate')"
        key="changeDate"
        prop="changeDate"
        :show-overflow-tooltip="true"
        label="变更日期"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.changeDate?parseTime(scope.row.changeDate,'{y}-{m}-{d}'):'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="创建日期"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        key="userName"
        prop="userName"
        label="创建人"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.userName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditTime')"
        key="auditTime"
        prop="auditTime"
        label="审核日期"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
           <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditorName')"
        key="auditorName"
        prop="auditorName"
        label="审核人"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('source')"
        key="source"
        prop="source"
        label="来源"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.source? systemTypeEnum.VL[scope.row.source]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditStatus')"
        key="auditStatus"
        prop="auditStatus"
        label="状态"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: '' }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
          <common-button icon="el-icon-s-check" type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 金额变更 -->
    <money-form ref="moneyRef" :audit-status="auditStatus" :project-id="projectId" v-model="moneyVisible" :detail-Info="detailInfo" :show-type="showType"/>
    <!-- 结算填报 -->
    <settle-form ref="settleRef" :audit-status="auditStatus" :project-id="projectId" v-model="settleVisible" :detail-Info="detailInfo" :show-type="showType"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/change-audit-log'
import { ref, watch, defineProps } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { auditTypeEnum, contractChangeTypeEnum, systemTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import moneyForm from '../money-form'
import settleForm from '../settle-form'

// crud交由presenter持有
const permission = {
  get: ['changeAuditLog:get'],
  editStatus: ['changeAuditLog:editStatus'],
  audit: ['changeAuditLog:audit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const moneyRef = ref()
const auditStatus = ref()
const showType = ref('detail')
const moneyVisible = ref(false)
const settleVisible = ref(false)
const detailInfo = ref({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更管理',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const { maxHeight } = useMaxHeight({
  wrapperBox: '.changeAuditLog',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true }
)

function openDetail(row, type) {
  auditStatus.value = row.auditStatus
  showType.value = type
  detailInfo.value = row
  switch (row.type) {
    // CONTRACT_INFO,CONTRACT_AMOUNT,CONTRACT_SETTLE
    case (contractChangeTypeEnum.CONTRACT_AMOUNT.V):
      moneyVisible.value = true
      break
    case (contractChangeTypeEnum.CONTRACT_SETTLE.V):
      settleVisible.value = true
      break
    default:
      break
  }
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
  return !!crud.query.projectId
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>

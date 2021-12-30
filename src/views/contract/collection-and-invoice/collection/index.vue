<template>
  <div class="app-container">
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
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="项目" min-width="250">
      <template v-slot="scope">
        <template v-if="scope.row.project">
          <div>{{ scope.row.project.serialNumber }}</div>
          <div>{{ scope.row.project.shortName }}</div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)" min-width="150">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('haveCollectionAmount')" key="haveCollectionAmount" prop="haveCollectionAmount" label="已收款金额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.haveCollectionAmount && scope.row.haveCollectionAmount>0? toThousand(haveCollectionAmount): scope.row.haveCollectionAmount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionAmount')" key="collectionAmount" prop="collectionAmount" label="收款金额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionReason')" key="collectionReason" prop="collectionReason" label="收款事由" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionMode')" key="collectionMode" prop="collectionMode" label="收款方式" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionDate')" key="collectionDate" prop="collectionDate" label="收款日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionUnit')" key="collectionUnit" prop="collectionUnit" :show-overflow-tooltip="true" label="收款单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionDepositBank')" key="collectionDepositBank" prop="collectionDepositBank" :show-overflow-tooltip="true" label="收款行" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionDepositBank }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionBankAccount')" key="collectionBankAccount" prop="collectionBankAccount" :show-overflow-tooltip="true" label="收款账号" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionBankAccount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentUnit')" key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.paymentUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentDepositBank')" key="paymentDepositBank" prop="paymentDepositBank" :show-overflow-tooltip="true" label="付款行" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.paymentDepositBank }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentBankAccount')" key="paymentBankAccount" prop="paymentBankAccount" :show-overflow-tooltip="true" label="付款账号" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.paymentBankAccount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('writtenByName')" key="writtenByName" prop="writtenByName" label="填报人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.writtenByName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="填报日期" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditorName')" key="auditorName" prop="auditorName" label="审核人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditorName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" label="审核日期" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditStatus')" key="auditStatus" prop="auditStatus" label="状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: ''}}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.edit,...permission.audit])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
        <common-button icon="el-icon-s-check" type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V"/>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  <mDetail :collectionInfo="currentInfo" :type="showType"  v-model="detailVisible" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/collection-and-invoice/collection'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { auditTypeEnum } from '@enum-ms/contract'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'

// crud交由presenter持有
const permission = {
  get: ['collection:get'],
  add: ['collection:add'],
  edit: ['collection:edit'],
  audit: ['collection:audit']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentInfo = ref({})
const showType = ref('detail')
const detailVisible = ref(false)
const dict = useDict(['payment_reason'])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '收款填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['haveCollectionAmount', 'collectionMode', 'collectionReason', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.projectId = v.project.id
    return v
  })
}

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
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

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
    <el-table-column v-if="columns.visible('orderSerialNumber')" key="orderSerialNumber" prop="orderSerialNumber" :show-overflow-tooltip="true" label="订单号" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.orderSerialNumber }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="所属订单" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.orderSerialNumber }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="供应商" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.supplierName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('propertyType')" key="propertyType" prop="propertyType" label="属性" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.propertyType? supplierPayMentTypeEnum.VL[scope.row.propertyType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('basicClassName')" key="basicClassName" prop="basicClassName" :show-overflow-tooltip="true" label="种类" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.basicClassName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" :show-overflow-tooltip="true" label="合同金额(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.amount? scope.row.amount.toThousand(): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('amount1')" key="amount1" prop="amount1" :show-overflow-tooltip="true" label="运费金额(元)" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.amount && scope.row.amount>0? scope.row.amount.toThousand(): scope.row.amount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('inBoundAmount')" key="inBoundAmount" :show-overflow-tooltip="true" prop="inBoundAmount" label="入库累计(元)" align="center" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.inBoundAmount && scope.row.inBoundAmount>0? scope.row.inBoundAmount.toThousand(): scope.row.inBoundAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('applyUserName')" key="applyUserName" :show-overflow-tooltip="true" prop="applyUserName" label="申请人" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.applyUserName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('applyDate')" key="applyDate" :show-overflow-tooltip="true" prop="applyDate" label="申请日期" align="center" width="100px">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.applyDate }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('applyAmount')" key="applyAmount" :show-overflow-tooltip="true" prop="applyAmount" label="申请金额(元)" align="center" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.applyAmount && scope.row.applyAmount>0? scope.row.applyAmount.toThousand(): scope.row.applyAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" :show-overflow-tooltip="true" prop="paymentAmount" label="付款金额(元)" align="center" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.paymentAmount && scope.row.paymentAmount>0? scope.row.paymentAmount.toThousand(): scope.row.paymentAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('payRate')" key="payRate" prop="payRate" :show-overflow-tooltip="true" label="付款比例" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.paymentAmount && scope.row.amount? (scope.row.paymentAmount/scope.row.amount)*100+'%' :'' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentReason')" key="paymentReason" :show-overflow-tooltip="true" prop="paymentReason" label="付款事由" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.paymentReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.paymentReason]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('payForType')" key="payForType" :show-overflow-tooltip="true" prop="payForType" label="费用类别" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.payForType? contractPayForEnum.VL[scope.row.payForType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('payType')" key="payType" prop="payType" :show-overflow-tooltip="true" label="付款方式" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.payType? supplierPayModeEnum.VL[scope.row.payType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentDate')" key="paymentDate" :show-overflow-tooltip="true" prop="paymentDate" label="付款日期" align="center" min-width="100">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.paymentDate }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentUnit')" key="paymentUnit" :show-overflow-tooltip="true" prop="paymentUnit" label="付款单位" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.paymentUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentBank')" key="paymentBank" :show-overflow-tooltip="true" prop="paymentBank" label="付款行" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.paymentBank }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentBankAccount')" key="paymentBankAccount" :show-overflow-tooltip="true" prop="paymentBankAccount" label="付款账号" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.paymentBankAccount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveUnit')" key="receiveUnit" prop="receiveUnit" :show-overflow-tooltip="true" label="收款单位" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.receiveUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveBank')" key="receiveBank" prop="receiveBank" :show-overflow-tooltip="true" label="收款行" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.receiveBank }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveBankAccount')" key="receiveBankAccount" prop="receiveBankAccount" :show-overflow-tooltip="true" label="收款账号" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.receiveBankAccount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectList')" key="projectList" prop="projectList" :show-overflow-tooltip="true" label="关联项目" align="center" min-width="100">
      <template v-slot="scope">
        <template v-if="scope.row.projectList && scope.row.projectList.length>0">
          <div v-for="item in scope.row.projectList" :key="item.id">{{ item.serialNumber+' '+item.shortName }}</div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('writtenByName')" key="writtenByName" prop="writtenByName" :show-overflow-tooltip="true" label="填报人" align="center" width="80px">
      <template v-slot="scope">
        <div>{{ scope.row.writtenByName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" :show-overflow-tooltip="true" label="填报日期" align="center" width="100px">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditUserName')" key="auditUserName" prop="auditUserName" :show-overflow-tooltip="true" label="审核人" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.auditUserName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" :show-overflow-tooltip="true" label="审核日期" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.auditTime }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditStatus')" key="auditStatus" prop="auditStatus" label="状态" align="center" width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: ''}}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('source')" key="source" prop="source" label="来源" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.source? systemTypeEnum.VL[scope.row.source]: '' }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
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
  <mDetail :paymentDetailInfo="currentInfo" :type="showType"  v-model="detailVisble" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-invoice/pay'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { auditTypeEnum, systemTypeEnum, supplierPayMentTypeEnum, contractPayForEnum, supplierPayModeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import useDict from '@compos/store/use-dict'
import { toThousand } from '@/utils/data-type/number'

// crud交由presenter持有
const permission = {
  get: ['supplierPayment:get'],
  add: ['supplierPayment:add'],
  edit: ['supplierPayment:edit'],
  audit: ['supplierPayment:audit']
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
const detailVisble = ref(false)
const dict = useDict(['payment_reason'])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '付款填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['serialNumber','amount', 'amount1', 'inBoundAmount', 'applyUserName', 'applyDate', 'applyAmount', 'payRate', 'paymentReason', 'payType', 'paymentBank', 'paymentBankAccount','projectList','auditUserName','auditTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplierPayment',
  paginate: true,
  extraHeight: 157
})

function openDetail(row,type){
  currentInfo.value = row
  showType.value = type
  detailVisble.value = true
}

CRUD.HOOK.handleRefresh = (crud,data)=>{
  // data.data.content 
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

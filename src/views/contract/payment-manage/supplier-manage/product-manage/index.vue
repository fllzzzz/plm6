<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType"/>
    </div>
    <!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="[{}]"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="50" type="index" fixed="left"/>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" :show-overflow-tooltip="true" label="订单号" width="80" align="center" fixed="left">
      <template v-slot="scope">
        <span>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('signingDate')" key="signingDate" prop="signingDate" :show-overflow-tooltip="true" label="签订日期" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.signingDate? parseTime(scope.row.signingDate,'{y}-{m}-{d}'):'-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="供应商" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? scope.row.businessTypeEnum: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectManagerName')" key="projectManagerName" prop="projectManagerName" :show-overflow-tooltip="true" label="种类" align="center" width="90">
      <template v-slot="scope">
        <div>{{ scope.row.projectManagerName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" label="合同额" align="center" min-width="130px">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '-' }}</span>
         <!-- <el-tag @click="openContractMoney(scope.row.id)" effect="plain">{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '-' }}</el-tag> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount" label="结算额" align="center" min-width="130px">
      <template v-slot="scope">
        <el-tag effect="plain">{{ scope.row.settlementAmount? toThousand(scope.row.settlementAmount): '-' }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount" label="入库额" align="center" min-width="130px">
      <template v-slot="scope">
        <el-tag effect="plain" @click="openStockAmount(scope.row.id)">{{ scope.row.settlementAmount? toThousand(scope.row.settlementAmount): '-' }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionAmount')" key="collectionAmount" prop="collectionAmount" label="付款额" align="center" min-width="130px">
      <template v-slot="scope">
        <el-tag @click="openTab(scope.row.id,'receive')" effect="plain">{{ isNotBlank(scope.row.collectionAmount)? toThousand(scope.row.collectionAmount): '-' }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionRate')" key="collectionRate" prop="collectionRate" label="付款比例" align="center" min-width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.collectionRate? scope.row.collectionRate*100+'%': '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="收票额" align="center" min-width="130px">
      <template v-slot="scope">
        <el-tag @click="openTab(scope.row.id,'invoice')" effect="plain">{{ isNotBlank(scope.row.invoiceAmount)? toThousand(scope.row.invoiceAmount): '-' }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" min-width="100px">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceRate? scope.row.invoiceRate*100+'%': '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="90px">
      <template v-slot="scope">
        <el-tag :type="scope.row.status===projectStatusEnum.SETTLED.V?'success':'warning'" effect="plain">{{ scope.row.status? projectStatusEnum.VL[scope.row.status]:'-' }}</el-tag>
      </template>
    </el-table-column>
  </common-table>
  <!-- 合同额 -->
  <contract-money v-model="moneyVisible" :projectId="currentProjectId"/>
  <!-- 发生额 -->
  <stock-amount v-model="stockVisible"/>
  <!-- 收付款 -->
  <receiveAndInvoice v-model="tabVisible" :projectId="currentProjectId" :tabName="activeName"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/contract-ledger'
import { ref } from 'vue'
import { contractSupplierProductPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { projectTypeEnumN, projectStatusEnum } from '@enum-ms/contract'
import stockAmount from './module/stock-amount'
import contractMoney from './module/contract-money'
import receiveAndInvoice from './module/receive-and-invoice'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { isNotBlank } from '@data-type/index'

const { currentProjectType } = mapGetters(['currentProjectType'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const moneyVisible = ref(false)
const stockVisible = ref(false)
const tabVisible = ref(false)
const currentProjectId = ref()
const activeName = ref('receive')
const { crud, columns } = useCRUD(
  {
    title: '制成品',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.productManage',
  paginate: true,
  extraHeight: 40
})

// function openContractMoney(row) {
//   currentProjectId.value = row
//   moneyVisible.value = true
// }

function openStockAmount(row) {
  if (!checkPermission(permission.inbound.get)) {
    return
  }
  currentProjectId.value = row
  stockVisible.value = true
}

function openTab(row, name) {
  activeName.value = name
  currentProjectId.value = row
  tabVisible.value = true
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
::v-deep(.el-table .cell){
  padding-left:2px;
  padding-right:2px;
}
::v-deep(.el-tag--small){
  padding:0 3px;
}
</style>

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
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
    return-source-data
    :showEmptySymbol="false"
  >
    <el-table-column prop="index" label="序号" align="center" width="50" type="index" fixed="left"/>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" :show-overflow-tooltip="true" label="项目类型" width="80" align="center" fixed="left">
      <template v-slot="scope">
        <span>{{ scope.row.projectType? projectTypeEnum.VL[scope.row.projectType]: '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="columns.visible('project.shortName')"
      key="project.shortName"
      prop="project.shortName"
      :show-overflow-tooltip="true"
      label="项目"
      min-width="150"
      align="center"
      fixed="left"
    >
      <template v-slot="scope">
        <span class="project-name">{{ projectNameFormatter(scope.row) }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="业务类型" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectManagerName')" key="projectManagerName" prop="projectManagerName" :show-overflow-tooltip="true" label="业务负责人" align="center" width="90">
      <template v-slot="scope">
        <div>{{ scope.row.projectManagerName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('signingDate')" key="signingDate" prop="signingDate" :show-overflow-tooltip="true" label="签订日期" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.signingDate? parseTime(scope.row.signingDate,'{y}-{m}-{d}'):'-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" label="合同额" align="center">
      <template v-slot="scope">
        <div @click="openContractMoney(scope.row.id)" style="cursor:pointer;color:#409eff;text-align:right;">{{ isNotBlank(scope.row.contractAmount)? toThousand(scope.row.contractAmount): '-' }}</div>
        <!-- <el-tag @click="openContractMoney(scope.row.id)" effect="plain" style="width:100%;text-align:right;">{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '-' }}</el-tag> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount" label="结算额" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.settlementAmount? toThousand(scope.row.settlementAmount): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionAmount')" key="collectionAmount" prop="collectionAmount" label="累计收款" align="center">
      <template v-slot="scope">
        <div @click="openTab(scope.row,'collection')" style="cursor:pointer;color:#409eff;text-align:right;">{{ isNotBlank(scope.row.collectionAmount)? toThousand(scope.row.collectionAmount): '-' }}</div>
        <!-- <el-tag @click="openTab(scope.row.id,'collection')" effect="plain" style="width:100%;text-align:right;">{{ isNotBlank(scope.row.collectionAmount)? toThousand(scope.row.collectionAmount): '-' }}</el-tag> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionRate')" key="collectionRate" prop="collectionRate" label="收款比例" align="center" width="80px">
      <template v-slot="scope">
        <div>{{ scope.row.collectionRate? scope.row.collectionRate*100+'%': '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="累计开票" align="center">
      <template v-slot="scope">
        <div @click="openTab(scope.row,'invoice')" style="cursor:pointer;color:#409eff;text-align:right;">{{ isNotBlank(scope.row.invoiceAmount)? toThousand(scope.row.invoiceAmount): '-' }}</div>
        <!-- <el-tag @click="openTab(scope.row.id,'invoice')" effect="plain" style="width:100%;text-align:right;">{{ isNotBlank(scope.row.invoiceAmount)? toThousand(scope.row.invoiceAmount): '-' }}</el-tag> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="开票比例" align="center" width="80px">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceRate? scope.row.invoiceRate*100+'%': '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('deliverInstallAmount')" key="deliverInstallAmount" prop="deliverInstallAmount" label="累计发生额" align="center">
      <template v-slot="scope">
        <div @click="openOccurAmount(scope.row.id)" style="cursor:pointer;color:#409eff;text-align:right;">{{ isNotBlank(scope.row.deliverInstallAmount)? toThousand(scope.row.deliverInstallAmount): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('availableBalance')" key="availableBalance" prop="availableBalance" label="可用余额" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.availableBalance? toThousand(scope.row.availableBalance): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="项目状态" align="center" width="80px">
      <template v-slot="scope">
        <el-tag :type="scope.row.status===projectStatusEnum.SETTLED.V?'success':'warning'" effect="plain">{{ scope.row.status? projectStatusEnum.VL[scope.row.status]:'-' }}</el-tag>
      </template>
    </el-table-column>
  </common-table>
  <!-- 合同额 -->
  <contract-money v-model="moneyVisible" :projectId="currentProjectId"/>
  <!-- 发生额 -->
  <occur-amount v-model="occurVisible" :projectId="currentProjectId"/>
  <!-- 收付款 -->
  <collectionAndInvoice v-model="tabVisible" :projectId="currentProjectId" :tabName="activeName" @success="crud.toQuery" :current-row="currentRow"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/contract-ledger'
import { ref } from 'vue'
import { contractLedgerPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { businessTypeEnum, projectTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { projectNameFormatter } from '@/utils/project'
import occurAmount from './module/occur-amount'
import contractMoney from './module/contract-money'
import collectionAndInvoice from './module/collection-and-invoice'
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
const occurVisible = ref(false)
const tabVisible = ref(false)
const currentProjectId = ref()
const currentRow = ref({})
const activeName = ref('collection')
const { crud, columns } = useCRUD(
  {
    title: '项目台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractLedger',
  paginate: true,
  extraHeight: 40
})

function openContractMoney(row) {
  if (!checkPermission(permission.amountLog)) {
    return
  }
  currentProjectId.value = row
  moneyVisible.value = true
}

function openOccurAmount(row) {
  if (!checkPermission(permission.occurLog)) {
    return
  }
  currentProjectId.value = row
  occurVisible.value = true
}

function openTab(row, name) {
  activeName.value = name
  currentProjectId.value = row.id
  currentRow.value = row
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
  padding-left:4px;
  padding-right:4px;
}
::v-deep(.el-tag--small){
  padding:0 3px;
}
</style>

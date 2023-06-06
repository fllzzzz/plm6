<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="columnsDataFormat"
    >
      <el-table-column prop="index" label="序号" align="center" width="70" type="index" fixed="left">
        <template v-slot="{ row, $index }">
          <table-cell-tag
            :show="row.status === projectStatusEnum.SETTLED.V"
            :offset="10"
            :name="projectStatusEnum.SETTLED.L"
            color="#36ae81"
          />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        show-overflow-tooltip
        label="项目"
        min-width="180"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('customerUnit')"
        align="center"
        key="customerUnit"
        prop="customerUnit"
        show-overflow-tooltip
        label="业主名称"
        min-width="150"
      />
      <el-table-column
        v-if="columns.visible('businessType')"
        align="center"
        key="businessType"
        prop="businessType"
        show-overflow-tooltip
        label="业务类型"
        width="80"
      />
      <el-table-column
        v-if="columns.visible('signingDate')"
        align="center"
        key="signingDate"
        prop="signingDate"
        show-overflow-tooltip
        label="签订日期"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('deptName')"
        align="center"
        key="deptName"
        prop="deptName"
        show-overflow-tooltip
        label="签订部门"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('projectManager')"
        align="center"
        key="projectManager"
        prop="projectManager"
        show-overflow-tooltip
        label="项目经理"
        min-width="90"
      />
      <el-table-column
        v-if="columns.visible('contractAmount')"
        align="right"
        key="contractAmount"
        prop="contractAmount"
        show-overflow-tooltip
        label="合同金额"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('settlementAmount')"
        align="right"
        key="settlementAmount"
        prop="settlementAmount"
        show-overflow-tooltip
        label="结算额"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('costAmount')"
        align="right"
        key="costAmount"
        prop="costAmount"
        show-overflow-tooltip
        label="综合成本"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-if="checkPermission(permission.detail)" style="color: #ff5600" class="pointer" @click="openDetail('compositeCost', row)">
            {{ row.costAmount }}
          </span>
          <span v-else>{{ row.costAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossProfit')"
        align="right"
        key="grossProfit"
        prop="grossProfit"
        show-overflow-tooltip
        label="毛利润"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('grossProfitRate')"
        align="center"
        key="grossProfitRate"
        prop="grossProfitRate"
        show-overflow-tooltip
        label="利润率"
        width="80"
      />
      <el-table-column
        v-if="columns.visible('exportTaxRebate')"
        align="right"
        key="exportTaxRebate"
        prop="exportTaxRebate"
        show-overflow-tooltip
        label="出口退税"
        min-width="100"
      >
        <template #default="{ row }">
          <span
            v-if="checkPermission(permission.detail)"
            style="color: #ff5600"
            class="pointer"
            @click="openDetail('availableBalance', row)"
          >
            {{ row.exportTaxRebate }}
          </span>
          <span v-else>{{ row.exportTaxRebate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('collectionAmount')"
        align="right"
        key="collectionAmount"
        prop="collectionAmount"
        show-overflow-tooltip
        label="收款金额"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-if="checkPermission(permission.detail)" style="color: #0079ff" class="pointer" @click="openDetail('collection', row)">
            {{ row.collectionAmount }}
          </span>
          <span v-else>{{ row.collectionAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unpaidAmount')"
        align="right"
        key="unpaidAmount"
        prop="unpaidAmount"
        show-overflow-tooltip
        label="未付金额"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('collectionRate')"
        align="center"
        key="collectionRate"
        prop="collectionRate"
        show-overflow-tooltip
        label="收款率"
        width="80"
      />
      <el-table-column
        v-if="columns.visible('availableBalance')"
        align="right"
        key="availableBalance"
        prop="availableBalance"
        show-overflow-tooltip
        label="可用余额"
        min-width="100"
      >
        <template #default="{ row }">
          <span
            v-if="checkPermission(permission.detail)"
            style="color: #ff5600"
            class="pointer"
            @click="openDetail('availableBalance', row)"
          >
            {{ row.availableBalance }}
          </span>
          <span v-else>{{ row.availableBalance }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('happenedAmount')"
        align="right"
        key="happenedAmount"
        prop="happenedAmount"
        show-overflow-tooltip
        label="累计发货额"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-if="checkPermission(permission.detail)" style="color: #0079ff" class="pointer" @click="openDetail('happened', row)">
            {{ row.happenedAmount }}
          </span>
          <span v-else>{{ row.happenedAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('periodExpense')"
        align="right"
        key="periodExpense"
        prop="periodExpense"
        show-overflow-tooltip
        label="期间费用"
        min-width="100"
      >
        <template #default="{ row }">
          <span
            v-if="checkPermission(permission.detail)"
            style="color: #0079ff"
            class="pointer"
            @click="
              openDetail('costAscription', {
                ...row,
                costAscriptionEnum: costAscriptionEnum.PERIOD_COSTS.V,
                costAscriptionAmount: row.sourceRow.periodExpense,
              })
            "
          >
            {{ row.periodExpense }}
          </span>
          <span v-else>{{ row.periodExpense }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('invoiceAmount')"
        align="right"
        key="invoiceAmount"
        prop="invoiceAmount"
        show-overflow-tooltip
        label="开票金额"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-if="checkPermission(permission.detail)" style="color: #0079ff" class="pointer" @click="openDetail('invoice', row)">
            {{ row.invoiceAmount }}
          </span>
          <span v-else>{{ row.invoiceAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectRetention')"
        align="right"
        key="projectRetention"
        prop="projectRetention"
        show-overflow-tooltip
        label="项目留存"
        min-width="100"
      >
        <template #default="{ row }">
          <span
            v-if="checkPermission(permission.detail)"
            style="color: #0079ff"
            class="pointer"
            @click="
              openDetail('costAscription', {
                ...row,
                costAscriptionEnum: costAscriptionEnum.PROJECT_RETENTION.V,
                costAscriptionAmount: row.sourceRow.projectRetention,
              })
            "
          >
            {{ row.projectRetention }}
          </span>
          <span v-else>{{ row.projectRetention }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <invoice-record v-model="invoiceVisible" :detail-row="detailRow" />
    <collection-record v-model="collectionVisible" :detail-row="detailRow" />
    <happened-record v-model="happenedVisible" :detail-row="detailRow" />
    <available-balance v-model="availableBalanceVisible" :detail-row="detailRow" />
    <composite-cost v-model="compositeCostVisible" :detail-row="detailRow" />
    <cost-ascription v-model="costAscriptionVisible" :detail-row="detailRow" />
    <cost-page-dialog :detail-row="detailRow" v-model:visible="drawerVisible" :settlementStatus="crud.query.settlementStatus" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi from '@/api/contract/fortune-report/fortune-report'

import { fortuneReportPM as permission } from '@/page-permission/contract'
import { businessTypeEnum, projectTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { costAscriptionEnum } from '@enum-ms/config'
import checkPermission from '@/utils/system/check-permission'
import { toFixed } from '@data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'
import costPageDialog from './cost-page-dialog/index'
import invoiceRecord from './module/invoice-record'
import collectionRecord from './module/collection-record'
import happenedRecord from './module/happened-record'
import availableBalance from './module/available-balance'
import compositeCost from './module/composite-cost'
import costAscription from './module/cost-ascription'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const invoiceVisible = ref(false)
const collectionVisible = ref(false)
const happenedVisible = ref(false)
const availableBalanceVisible = ref(false)
const compositeCostVisible = ref(false)
const costAscriptionVisible = ref(false)
const detailRow = ref({})
const drawerVisible = ref(false)
const columnsDataFormat = ref([
  ['contractAmount', 'to-thousand'],
  ['settlementAmount', 'to-thousand'],
  ['costAmount', 'to-thousand'],
  ['grossProfit', 'to-thousand'],
  ['collectionAmount', 'to-thousand'],
  ['unpaidAmount', ['to-thousand-ck', 'YUAN']],
  ['safetyBalance', ['to-thousand-ck', 'YUAN']],
  ['retainedProfit', ['to-thousand-ck', 'YUAN']],
  ['happenedAmount', 'to-thousand'],
  ['availableBalance', 'to-thousand'],
  ['periodExpense', 'to-thousand'],
  ['invoiceAmount', 'to-thousand'],
  ['projectRetention', 'to-thousand'],
  ['exportTaxRebate', 'to-thousand'],
  ['grossProfitRate', ['suffix', '%']],
  ['collectionRate', ['suffix', '%']],
  ['signingDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['businessType', ['parse-enum', businessTypeEnum]],
  ['projectType', ['parse-enum', projectTypeEnum]]
])

const { crud, CRUD, columns } = useCRUD(
  {
    title: '业财报表',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  paginate: true
})

// 打开详情
function openDetail(type, row) {
  if (type === 'invoice') {
    invoiceVisible.value = true
  } else if (type === 'collection') {
    collectionVisible.value = true
  } else if (type === 'happened') {
    happenedVisible.value = true
  } else if (type === 'availableBalance') {
    availableBalanceVisible.value = true
  } else if (type === 'compositeCost') {
    compositeCostVisible.value = true
  } else if (type === 'costAscription') {
    costAscriptionVisible.value = true
  }
  detailRow.value = row
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content?.map((v) => {
    if (v.grossProfitRate) {
      v.grossProfitRate = toFixed(v.grossProfitRate * 100, 2)
    }
    v.unpaidAmount = v.contractAmount - v.collectionAmount
    v.collectionRate = toFixed((v.collectionAmount / v.contractAmount) * 100, 2)
    // 安全发货余额 = （合同金额 - 累计发货额）* 30%
    v.safetyBalance = (v.contractAmount - v.happenedAmount) * 0.3
    // 净利润 = 收款金额 - 综合成本 - 期间费用
    v.retainedProfit = v.collectionAmount ? v.collectionAmount - v.costAmount - v.periodExpense : '-'
    return v
  })
}
</script>

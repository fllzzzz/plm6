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
      row-key="id"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
       <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="140"
      >
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('businessType')"
        align="center"
        key="businessType"
        prop="businessType"
        :show-overflow-tooltip="true"
        label="业务类型"
      >
        <template v-slot="scope">
          <span>{{ businessTypeEnum.VL[scope.row.businessType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('orderType')"
        align="center"
        key="orderType"
        prop="orderType"
        :show-overflow-tooltip="true"
        label="订单属性"
      >
        <template v-slot="scope">
          <span>{{ orderSourceTypeEnum.VL[scope.row.orderSourceType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('status')"
        align="center"
        key="status"
        prop="status"
        :show-overflow-tooltip="true"
        label="状态"
      >
        <template v-slot="scope">
          <el-tag :type="projectStatusEnum.V[scope.row.status].TAG">{{ projectStatusEnum.VL[scope.row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('contractAmount')"
        align="center"
        key="contractAmount"
        prop="contractAmount"
        :show-overflow-tooltip="true"
        label="合同额"
      >
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.contractAmount,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('settlementAmount')"
        align="center"
        key="settlementAmount"
        prop="settlementAmount"
        :show-overflow-tooltip="true"
        label="结算额"
      >
        <template v-slot="scope">
          <span style="color: #409EFF;cursor: pointer" @click="showSettlementDetail(scope.row)">{{ toThousand(scope.row.settlementAmount,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('costAmount')"
        align="center"
        key="costAmount"
        prop="costAmount"
        :show-overflow-tooltip="true"
        label="综合成本"
      >
        <template v-slot="scope">
          <span style="color: #F56C6C;cursor: pointer" @click="showCostDetail(scope.row)">{{ toThousand(scope.row.costAmount,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossProfit')"
        align="center"
        key="grossProfit"
        prop="grossProfit"
        :show-overflow-tooltip="true"
        label="毛利润"
      >
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.grossProfit,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossProfitRate')"
        align="center"
        key="grossProfitRate"
        prop="grossProfitRate"
        :show-overflow-tooltip="true"
        label="利润率（%）"
      >
        <template v-slot="scope">
          <span>{{ (scope.row.grossProfitRate * 100).toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" v-if="columns.visible('collectionAmount1')" key="collectionAmount1" prop="collectionAmount1" :show-overflow-tooltip="true" label="收款">
        <el-table-column v-if="columns.visible('collectionAmount')" align="center" key="collectionAmount" prop="collectionAmount" :show-overflow-tooltip="true" label="收入">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.collectionAmount,decimalPrecision.contract) }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('receivableAmount')" align="center" key="receivableAmount" prop="receivableAmount" :show-overflow-tooltip="true" label="应收款">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.receivableAmount,decimalPrecision.contract) }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('availableBalance')" align="center" key="availableBalance" prop="availableBalance" :show-overflow-tooltip="true" label="可用余额">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.availableBalance,decimalPrecision.contract) }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('happenedAmount')"
        align="center"
        key="happenedAmount"
        prop="happenedAmount"
        :show-overflow-tooltip="true"
        label="累计发生额"
      >
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.happenedAmount,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <common-dialog title="结算详情" v-model="dialogVisible" width="460px" show-close custom-class="settlement-detail" top="10vh">
      <el-descriptions :column="1" :data="list" border style="cursor: pointer">
        <el-descriptions-item label="结算额" label-align="center" align="center">
          <span> {{ toThousand(list.settlementAmount,decimalPrecision.contract) }}</span>
        </el-descriptions-item>
        <el-descriptions-item label="附件" label-align="center" align="center">
          <template v-if="list.settlementAttachments?.length > 0">
            <div v-for="item in list.settlementAttachments" :key="item.id" style="margin-bottom:3px;">
              {{ item.name }}
              <export-button :params="{ id: item.id }" />
            </div>
          </template>
        </el-descriptions-item>
      </el-descriptions>
    </common-dialog>
    <cost-page-dialog :detail-row="detailRow" v-model:visible="drawerVisible" :settlementStatus="crud.query.settlementStatus" />
  </div>
</template>
<script setup>
import { ref, provide } from 'vue'
import crudApi from '@/api/contract/fortune-report/fortune-report'
import pagination from '@crud/Pagination'
import checkPermission from '@/utils/system/check-permission'
import { fortuneReportPM as permission } from '@/page-permission/contract'
import { projectNameFormatter } from '@/utils/project'
import { businessTypeEnum, orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { settlementStatusEnum } from '@/utils/enum/modules/finance'
import { toThousand } from '@data-type/number'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import useProjectTree from '@compos/store/use-project-tree'
import ExportButton from '@comp-common/export-button/index.vue'
import mHeader from './module/header.vue'
import costPageDialog from './cost-page-dialog/index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { decimalPrecision } = useDecimalPrecision()
const { projectTree } = useProjectTree()

provide('projectTree', projectTree)

const tableRef = ref()
const list = ref({})
const detailRow = ref({})
const dialogVisible = ref(false)
const drawerVisible = ref(false)
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
  extraBox: ['.head-container'],
  paginate: true
})

// 结算详情
function showSettlementDetail(row) {
  if (!checkPermission(permission.settleDetail)) {
    return false
  }
  dialogVisible.value = true
  list.value = row
}

// 成本页面详情
function showCostDetail(row) {
  if (!checkPermission(permission.cost.get)) {
    return false
  }
  drawerVisible.value = true
  detailRow.value = row.sourceRow
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  projectTree.value = projectTree.value.map(p => {
    p.serialNumberName = p.serialNumber + ' ' + p.name
    return p
  })
  res.data.content = res.data.content?.map(v => {
    if (v.settlementStatus === settlementStatusEnum.SETTLED.V) {
      v.status = projectStatusEnum.SETTLED.V
    }
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>

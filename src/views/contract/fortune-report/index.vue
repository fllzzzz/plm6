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
        v-if="columns.visible('name')"
        align="center"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('projectType')"
        align="center"
        key="projectType"
        prop="projectType"
        :show-overflow-tooltip="true"
        label="业务类型"
      >
        <template v-slot="scope">
          <span>{{ businessTypeEnum.VL[scope.row.projectType] }}</span>
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
          <span>{{ toThousand(scope.row.contractAmount) }}</span>
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
          <span style="color: #409EFF;cursor: pointer" @click="showSettlementDetail(scope.row)">{{ toThousand(scope.row.settlementAmount) }}</span>
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
          <span style="color: #F56C6C;cursor: pointer" @click="showCostDetail(scope.row)">{{ toThousand(scope.row.costAmount) }}</span>
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
          <span>{{ toThousand(scope.row.grossProfit) }}</span>
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
      <el-table-column align="center" key="collectionAmount" prop="collectionAmount" :show-overflow-tooltip="true" label="收款">
        <el-table-column align="center" key="collectionAmount" prop="collectionAmount" :show-overflow-tooltip="true" label="收款">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.collectionAmount) }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="receivableAmount" prop="receivableAmount" :show-overflow-tooltip="true" label="应收款">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.receivableAmount) }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="availableBalance" prop="availableBalance" :show-overflow-tooltip="true" label="可用余额">
          <template v-slot="scope">
            <span>{{ toThousand(scope.row.availableBalance) }}</span>
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
          <span>{{ toThousand(scope.row.happenedAmount) }}</span>
        </template>
      </el-table-column>
    </common-table>
    <common-dialog title="结算详情" v-model="dialogVisible" width="400px" show-close custom-class="settlement-detail" top="10vh">
      <el-descriptions :column="1" :data="list" border style="cursor: pointer">
        <el-descriptions-item label="结算额" label-align="center" align="center">
          <span> {{ toThousand(list.settlementAmount) }}</span>
        </el-descriptions-item>
        <el-descriptions-item label="附件" label-align="center" align="center">
          <el-icon :size="20" color="#409EFC" style="vertical-align: middle">
            <Link />
          </el-icon>
          <span>{{ list.cutInstructionId }} 结算单.png</span>
        </el-descriptions-item>
      </el-descriptions>
    </common-dialog>
    <cost-page-dialog :detail-row="detailRow" v-model:visible="drawerVisible" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import { businessTypeEnum, orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { Link } from '@element-plus/icons'
import crudApi from '@/api/contract/fortune-report/fortune-report'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header.vue'
import costPageDialog from './cost-page-dialog/index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

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
    // permission: { ...permission },
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
  dialogVisible.value = true
  list.value = row
}

// 成本页面详情
function showCostDetail(row) {
  drawerVisible.value = true
  detailRow.value = row.sourceRow
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map(v => {
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>

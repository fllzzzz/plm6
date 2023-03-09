<template>
  <div class="report-material-outbound-details app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      highlight-current-row
      @sort-change="crud.handleSortChange"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="!basicClass" :basic-class="row.basicClass" :row="row" />
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" show-outbound-mode show-classification classify-name-alias="名称" fixed="left">
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('outboundReceipt.outboundTime')"
            key="outboundReceipt.outboundTime"
            :show-overflow-tooltip="true"
            prop="outboundReceipt.outboundTime"
            label="出库时间"
            align="center"
            width="125"
            fixed="left"
            sortable="custom"
          >
            <template #default="{ row }">
              <span v-parse-time="row.outboundReceipt.outboundTime" />
            </template>
          </el-table-column>
        </template>
      </material-base-info-columns>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns :columns="columns" show-unit-price-e show-invoice-type :show-amount="amountCfg" :show-amount-excluding-v-a-t="amountExcludingVATCfg" />
      </template>
      <warehouse-info-columns :columns="columns" show-project show-monomer show-area show-workshop />
      <el-table-column
        v-if="columns.visible('outboundReceipt.serialNumber')"
        key="outboundReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.serialNumber"
        min-width="155"
        label="出库单号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['OUTBOUND']" :receipt="row.outboundReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundReceipt.applicantName')"
        key="outboundReceipt.applicantName"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.applicantName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('recipientName')"
        key="recipientName"
        :show-overflow-tooltip="true"
        prop="recipientName"
        label="领料人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('outboundReceipt.reviewerName')"
        key="outboundReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('outboundReceipt.createTime')"
        key="outboundReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      />
      <el-table-column
        v-if="columns.visible('outboundReceipt.reviewTime')"
        key="outboundReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.reviewTime"
        label="审核时间"
        align="center"
        width="125"
      />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="150px" />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/outbound'
import { reportRawMaterialOutboundDetailsPM as permission } from '@/page-permission/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()

// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialHasAmountColumns,
  ['outboundReceipt.outboundTime', 'parse-time'],
  ['outboundReceipt.reviewTime', 'parse-time'],
  ['outboundReceipt.createTime', 'parse-time']
])

// 报表配置
const { reportCfg } = useWmsConfig()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '出库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'outboundReceipt.applicantName',
      'outboundReceipt.reviewerName',
      'outboundReceipt.createTime',
      'outboundReceipt.reviewTime',
      'recipientName',
      'invoiceType',
      'taxRate'
    ],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 后台金额配置
const amountCfg = computed(() => !!reportCfg.value.amountShow)
const amountExcludingVATCfg = computed(() => !!reportCfg.value.amountExcludingTAXShow)

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount) && (amountCfg.value || amountExcludingVATCfg.value))

const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
  data.content.forEach((row) => {
    if (!row.outboundReceipt) row.outboundReceipt = {}
  })
  // 退货信息转换
  const rejectList = []
  data.content.forEach((row) => {
    if (Array.isArray(row.rejectList)) {
      row.rejectList.forEach((rr) => {
        rejectList.push(rr.material)
      })
    }
  })
  await setSpecInfoToList(rejectList)
  await numFmtByBasicClass(rejectList)
}
</script>

<style lang="scss" scoped>
.report-material-outbound-details {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>

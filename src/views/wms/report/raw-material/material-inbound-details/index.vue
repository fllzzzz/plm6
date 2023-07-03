<template>
  <div class="report-material-inbound-details app-container">
    <!--工具栏-->
    <m-header />
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
      <material-base-info-columns
        :columns="columns"
        :basic-class="basicClass"
        show-reject-status
        reject-detail-viewable
        show-classification
        classify-name-alias="名称"
        spec-merge
        sortable
        fixed="left"
      >
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('inboundTime')"
            key="inboundTime"
            :show-overflow-tooltip="true"
            prop="inboundTime"
            label="入库时间"
            align="center"
            width="125"
            fixed="left"
            sortable="custom"
          />
        </template>
      </material-base-info-columns>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns :columns="columns" show-unit-price-e show-invoice-type show-tax-rate :show-amount="amountCfg" :show-amount-excluding-v-a-t="amountExcludingVATCfg" />
      </template>
      <warehouse-info-columns :columns="columns" show-project />
      <el-table-column
        v-if="columns.visible('inboundReceipt.purchaseOrder')"
        key="inboundReceipt.purchaseOrder"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.purchaseOrder"
        label="采购合同编号"
        min-width="155"
      >
        <template #default="{ row }">
          <receipt-sn-clickable v-if="row.inboundReceipt" :receipt-types="['PURCHASE']" :receipt="row.inboundReceipt.purchaseOrder" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.serialNumber')"
        key="inboundReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.serialNumber"
        min-width="155"
        label="入库单号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['INBOUND']" :receipt="row.inboundReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.licensePlate')"
        key="inboundReceipt.licensePlate"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.licensePlate"
        label="车牌号"
        align="left"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.shipmentNumber')"
        key="inboundReceipt.shipmentNumber"
        prop="inboundReceipt.shipmentNumber"
        label="物流单号"
        align="left"
        min-width="150"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.supplier.name')"
        key="inboundReceipt.supplier.name"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.supplier.name"
        label="供应商"
        min-width="200"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.applicantName')"
        key="inboundReceipt.applicantName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.applicantName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.editorName')"
        key="inboundReceipt.editorName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.editorName"
        label="编辑人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.reviewerName')"
        key="inboundReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.createTime')"
        key="inboundReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          {{ row.inboundReceipt.createTime }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.reviewTime')"
        key="inboundReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.reviewTime"
        label="入库时间"
        align="center"
        width="125"
        fixed="left"
      >
        <template #default="{ row }">
          {{ row.inboundReceipt.reviewTime }}
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/inbound'
import { reportRawMaterialInboundDetailsPM as permission } from '@/page-permission/wms'
import checkPermission from '@/utils/system/check-permission'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

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
  ['inboundTime', 'parse-time'],
  ['inboundReceipt.reviewTime', 'parse-time'],
  ['inboundReceipt.createTime', 'parse-time']
])

// 报表配置
const { reportCfg } = useWmsConfig()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '入库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'inboundReceipt.applicantName',
      'inboundReceipt.editorName',
      'inboundReceipt.reviewerName',
      'inboundReceipt.createTime',
      'inboundReceipt.reviewTime',
      'inboundReceipt.licensePlate',
      'inboundReceipt.shipmentNumber',
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
    if (!row.inboundReceipt) row.inboundReceipt = {}
  })
  console.log(data.content)
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
.report-material-inbound-details {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>

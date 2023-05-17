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
    :data-format="dataFormat"
    :showEmptySymbol="false"
    :stripe="false"
    :summary-method="getSummaries"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('propertyType')" key="propertyType" prop="propertyType" :show-overflow-tooltip="true" align="center" label="订单类型" width="100" />
    <!-- <el-table-column v-if="columns.visible('project.serialNumber')" key="project.serialNumber" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="所属项目">
      <template v-slot="scope">
        <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
      </template>
    </el-table-column> -->
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="销售单位" align="center" min-width="140" />
    <el-table-column v-if="columns.visible('branchCompanyName')" key="branchCompanyName" prop="branchCompanyName" :show-overflow-tooltip="true" label="购方单位" align="center" min-width="140" />
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="收票日期" align="center" width="100" />
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="发票金额" align="right" min-width="100" />
    <el-table-column v-if="columns.visible('invoiceSerialNumber')" key="invoiceSerialNumber" prop="invoiceSerialNumber" :show-overflow-tooltip="true" label="发票号码" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceSerialNumber }}</div>
      </template>
    </el-table-column>
    <el-table-column key="attachments" prop="attachments" label="附件" align="center" :show-overflow-tooltip="true" min-width="140">
      <template #header>
        <el-tooltip effect="light" :content="`双击可预览附件`" placement="top">
          <div>
            <span>附件</span>
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template>
      <template v-slot="scope">
        <template v-if="scope.row.attachments && scope.row.attachments.length>0">
          <div v-for="item in scope.row.attachments" :key="item.id">
            <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
          </div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceType')" key="invoiceType" prop="invoiceType" label="发票类型" align="center" width="106" />
    <el-table-column v-if="columns.visible('taxRate')" key="taxRate" prop="taxRate" label="税率" align="center" width="60" />
  </common-table>
  <!--分页组件-->
  <pagination />
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/payment-ledger/pay-invoice'
import { ref } from 'vue'

import { contractSupplierPaymentLedgerPM } from '@/page-permission/contract'
import { supplierPayTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
// import { projectNameFormatter } from '@/utils/project'

// crud交由presenter持有
const permission = contractSupplierPaymentLedgerPM.invoice

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '收票台账',
    sort: [],
    permission: { ...permission },
    invisibleColumns: [],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.pay-invoice',
  paginate: true,
  extraHeight: 120
})

const dataFormat = ref([
  ['invoiceAmount', ['to-thousand-ck', 'YUAN']],
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['propertyType', ['parse-enum', supplierPayTypeEnum]],
  ['invoiceType', ['parse-enum', invoiceTypeEnum]],
  ['taxRate', ['suffix', '%']]
])

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime?.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  } else {
    crud.query.startDate = undefined
    crud.query.endDate = undefined
  }
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'invoiceAmount') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index].toFixed(2)
      }
    }
  })
  return sums
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

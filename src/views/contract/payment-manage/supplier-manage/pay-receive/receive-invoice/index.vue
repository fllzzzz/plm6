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
    :showEmptySymbol="false"
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
    <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" :show-overflow-tooltip="true" label="合同金额(元)" min-width="150">
      <template v-slot="scope">
        <span>{{ scope.row.amount? toThousand(scope.row.amount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectList')" key="projectList" prop="projectList" :show-overflow-tooltip="true" label="关联项目" align="center" min-width="100">
      <template v-slot="scope">
        <template v-if="scope.row.projectList && scope.row.projectList.length>0">
          <div v-for="item in scope.row.projectList" :key="item.id">{{ item.serialNumber+' '+item.shortName }}</div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('amount1')" key="amount1" prop="amount1" :show-overflow-tooltip="true" label="运费金额(元)" min-width="150">
      <template v-slot="scope">
        <span>{{ scope.row.amount? toThousand(scope.row.amount): '' }}</span>
      </template>
    </el-table-column>
     <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="发票面额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount): scope.row.invoiceAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceType')" key="invoiceType" prop="invoiceType" label="发票类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceType ? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('tax')" key="tax" prop="tax" label="进项税额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.tax && scope.row.tax>0? toThousand(scope.row.tax): scope.row.tax }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceSerialNumber')" key="invoiceSerialNumber" prop="invoiceSerialNumber" :show-overflow-tooltip="true" label="发票号码" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceSerialNumber }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveInvoiceUnit')" key="receiveInvoiceUnit" prop="receiveInvoiceUnit" :show-overflow-tooltip="true" label="收票单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.receiveInvoiceUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceUnit')" key="invoiceUnit" prop="invoiceUnit" :show-overflow-tooltip="true" label="开票单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceDate')" key="invoiceDate" prop="invoiceDate" label="发票日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceDate? parseTime(scope.row.invoiceDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('receiveInvoiceDate')" key="receiveInvoiceDate" prop="receiveInvoiceDate" label="收票日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.receiveInvoiceDate? parseTime(scope.row.receiveInvoiceDate,'{y}-{m}-{d}'): '-' }}</div>
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
        <div>{{ scope.row.auditTime }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditStatus')" key="auditStatus" prop="auditStatus" label="状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: ''}}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([...permission.edit,...permission.audit])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
        <common-button icon="el-icon-s-check" type="primary" size="mini" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V && checkPermission(permission.audit)"/>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  <mDetail :collectionInfo="currentInfo" :type="showType"  v-model="detailVisble" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-invoice/invoice'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { auditTypeEnum, invoiceTypeEnum, supplierPayMentTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'

// crud交由presenter持有
const permission = {
  get: ['supplierInvoice:get'],
  add: ['supplierInvoice:add'],
  edit: ['supplierInvoice:edit'],
  audit: ['supplierInvoice:audit']
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
const { crud, columns } = useCRUD(
  {
    title: '收票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['serialNumber', 'amount', 'amount1', 'projectList', 'tax', 'invoiceSerialNumber', 'receiveInvoiceDate', 'invoiceDate', 'auditTime', 'auditUserName'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplierInvoice',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisble.value = true
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

<template>
  <div>
    <!--表格渲染-->
    <div>
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      show-summary
      :summary-method="getSummaries"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column prop="receiveInvoiceDate" label="收票日期" align="center" width="100" show-overflow-tooltip>
        <template v-slot="scope">
          <div>{{ parseTime(scope.row.receiveInvoiceDate,'{y}-{m}-{d}') }}</div>
        </template>
      </el-table-column>
      <el-table-column key="invoiceAmount" prop="invoiceAmount" label="票面金额" align="center" width="160">
        <template v-slot="scope">
          <template v-if="scope.row.attachments && scope.row.attachments.length>0">
            <div v-for="item in scope.row.attachments" :key="item.id">
              <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{toThousand(scope.row.invoiceAmount)}}</div>
            </div>
          </template>
          <template v-else>{{toThousand(scope.row.invoiceAmount)}}</template>
        </template>
      </el-table-column>
      <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" align="center" width="120" >
        <template v-slot="scope">
          <div>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="80">
        <template v-slot="scope">
          <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V">{{ scope.row.taxRate? scope.row.taxRate+'%': '' }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="amountExcludingTax" label="不含税" align="center" min-width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{toThousand(row.amountExcludingTax)}}</span>
        </template>
      </el-table-column>
      <el-table-column prop="tax" label="税额" align="center" min-width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{toThousand(row.tax)}}</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceSerialNumber" label="发票编号" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column prop="branchCompanyName" label="购买方" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column prop="supplierName" label="销售方" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column prop="actualInvoiceUnit" label="实际销售方" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="applyUserName" prop="applyUserName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName }}</div>
        </template>
      </el-table-column>
       <el-table-column key="auditStatus" prop="auditStatus" label="审核状态" align="center">
        <template v-slot="scope">
          <template v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.audit)">
            <common-button type="primary" @click="openDetail(scope.row, 'audit')">审核</common-button>
          </template>
          <template v-else>
            <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
            <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          </template>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.audit,...permission.detail])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button v-if="checkPermission(permission.detail)" icon="el-icon-view" type="info" size="mini" @click="openDetail(scope.row, 'detail')"/>
          <udOperation :data="scope.row" :show-edit="scope.row.auditStatus!==auditTypeEnum.PASS.V?true:false" :show-del="scope.row.auditStatus!==auditTypeEnum.PASS.V?true:false" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  <mForm :detail-info="detailInfo" />
  <mDetail :detail-info="detailInfo" v-model="detailVisible" :currentRow="currentRow" :showType="showType" @success="handleSuccess"/>
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/jd-logistics-invoice'
import { ref, defineProps, watch, defineEmits } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import checkPermission from '@/utils/system/check-permission'
import { auditTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { contractSupplierLogisticsPM } from '@/page-permission/contract'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import mForm from './form'
import mHeader from './header'
import mDetail from './detail'
import udOperation from '@crud/UD.operation'

const permission = contractSupplierLogisticsPM.invoice
const emit = defineEmits(['success'])
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  visibleValue: {
    type: Boolean,
    default: false
  }
})
const tableRef = ref()
const currentRow = ref({})
const pdfShow = ref(false)
const currentId = ref()
const showType = ref('detail')
const detailVisible = ref(false)

const { CRUD, crud } = useCRUD(
  {
    title: '收票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['supplierId'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.pay-invoice',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.query.auditStatus = auditTypeEnum.AUDITING.V
      crud.query.supplierId = props.detailInfo.supplierId
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['invoiceAmount', 'tax', 'amountExcludingTax'],
    toThousandFields: ['invoiceAmount', 'tax', 'amountExcludingTax']
  })
}

function openDetail(row, type) {
  showType.value = type
  currentRow.value = row
  detailVisible.value = true
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content.map(v => {
    v.amountExcludingTax = v.taxRate ? (v.invoiceAmount / (1 + v.taxRate / 100)).toFixed(2) : v.invoiceAmount
    v.tax = (v.invoiceAmount - v.amountExcludingTax).toFixed(2)
  })
}

CRUD.HOOK.afterDelete = () => {
  emit('success')
}

CRUD.HOOK.afterAddSuccess = () => {
  emit('success')
}

function handleSuccess() {
  crud.toQuery()
  emit('success')
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
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>

<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <el-tag type="success" size="medium" v-if="currentRow.amount">{{`合同额:${toThousand(currentRow.amount)}`}}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      return-source-data
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="paymentDate" prop="paymentDate" label="*付款日期" align="center" width="160">
        <template v-slot="scope">
          <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="applyAmount1" prop="applyAmount1" label="*付款金额" align="center" min-width="170" class="money-column">
        <el-table-column key="applyAmount" prop="applyAmount" label="金额" align="center" min-width="85">
          <template v-slot="scope">
            <div>{{ scope.row.applyAmount && scope.row.applyAmount>0? toThousand(scope.row.applyAmount): scope.row.applyAmount }}</div>
          </template>
        </el-table-column>
        <el-table-column key="applyAmount2" prop="applyAmount2" label="大写" align="center" min-width="85">
          <template v-slot="scope">
            <div>{{scope.row.applyAmount?'('+digitUppercase(scope.row.applyAmount)+')':''}}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="paymentReasonId" prop="paymentReasonId" label="*付款事由" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.paymentReasonId]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentMethod" prop="paymentMethod" label="*付款方式" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.paymentMethod? paymentFineModeEnum.VL[scope.row.paymentMethod]: '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentBank" prop="paymentBank" :show-overflow-tooltip="true" label="*付款银行" align="center">
        <template v-slot="scope">
         <div>{{ scope.row.paymentBank? scope.row.paymentBank: '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="*收款单位" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.paymentUnit? scope.row.paymentUnit: '-'  }}</div>
        </template>
      </el-table-column>
      <el-table-column key="attachments" prop="attachments" label="附件" align="center" :show-overflow-tooltip="true">
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
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-invoice/pay'
import { ref, defineProps, watch } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useDict from '@compos/store/use-dict'
import { auditTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand, digitUppercase } from '@data-type/number'
import { contractSupplierMaterialPM } from '@/page-permission/contract'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const permission = contractSupplierMaterialPM.payment

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  visibleValue: {
    type: Boolean,
    default: false
  },
  propertyType: {
    type: [Number, String],
    default: undefined
  }
})

const tableRef = ref()
const dict = useDict(['payment_reason'])
const pdfShow = ref(false)
const currentId = ref()
const { crud } = useCRUD(
  {
    title: '付款记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['orderId', 'propertyType', 'auditStatus'],
    invisibleColumns: ['haveApplyAmount', 'collectionMode', 'collectionReason', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.material-payment',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.query.orderId = props.currentRow.id
      crud.query.propertyType = supplierPayTypeEnum.PURCHASE.V
      crud.query.auditStatus = auditTypeEnum.PASS.V
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
    props: ['applyAmount'],
    toThousandFields: ['applyAmount']
  })
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

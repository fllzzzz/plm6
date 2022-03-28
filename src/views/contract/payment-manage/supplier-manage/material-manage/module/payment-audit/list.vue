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
      :cell-class-name="wrongCellMask"
      return-source-data
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购订单" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="申请人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionDate" prop="collectionDate" label="申请日期" align="center" width="160">
        <template v-slot="scope">
          <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionAmount" prop="collectionAmount" label="申请金额" align="center" min-width="85">
        <template v-slot="scope">
          <div>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionReason" prop="collectionReason" label="*付款事由" align="center" width="120">
        <template v-slot="scope">
          <div>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditStatus" prop="auditStatus" label="状态" align="center" width="80px">
        <template v-slot="scope">
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':'warning'" effect="plain">{{ isNotBlank(scope.row.auditStatus)? auditTypeEnum.VL[scope.row.auditStatus]:'-' }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="actualAmount" prop="actualAmount" label="实付金额" align="center" min-width="85">
        <template v-slot="scope">
          <div>{{ scope.row.actualAmount && scope.row.actualAmount>0? toThousand(scope.row.actualAmount): scope.row.actualAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
     <el-table-column key="auditorDate" prop="auditorDate" label="审核日期" align="center" width="160">
        <template v-slot="scope">
          <div>{{ scope.row.auditorDate? parseTime(scope.row.auditorDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.detail,...permission.audit])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" v-permission="permission.detail" @click="openDetail(scope.row, 'detail')"/>
          <common-button icon="el-icon-s-check" type="primary" size="mini" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V && checkPermission(permission.audit)"/>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  <detail v-model="detailVisible" :showType="showType" :detailInfo="detailInfo"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-invoice/pay'
import { ref, defineProps, watch } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { contractSupplierMaterialPM } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useDict from '@compos/store/use-dict'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { validate } from '@compos/form/use-table-validate'
import { auditTypeEnum } from '@enum-ms/contract'
import { isNotBlank } from '@data-type/index'
import detail from './detail'
// import { ElMessage } from 'element-plus'

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
const detailInfo = ref({})
const showType = ref('detail')
const detailVisible = ref(false)
const { crud, CRUD } = useCRUD(
  {
    title: '付款审核',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const tableRules = {
  collectionDate: [{ required: true, message: '请选择付款日期', trigger: 'change' }],
  collectionAmount: [{ required: true, message: '请选择付款金额', trigger: 'change', type: 'number' }],
  collectionBankAccountId: [{ required: true, message: '请选择付款银行', trigger: 'change' }],
  collectionMode: [{ required: true, message: '请选择付款方式', trigger: 'change' }],
  collectionReason: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  paymentUnit: [{ required: true, message: '请输入付款单位', trigger: 'blur' }]
}
function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function openDetail(row, type) {
  detailInfo.value = row
  showType.value = type
  detailVisible.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.orderId = props.currentRow.id
  crud.query.propertyType = props.propertyType
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

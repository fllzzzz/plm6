<template>
  <div>
    <!--表格渲染-->
    <div>
      <el-tag type="success" size="medium" v-if="currentRow.freight">{{'运输额:'+toThousand(currentRow.freight,decimalPrecision.contract)}}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="applyUserName" prop="applyUserName" label="申请人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName? scope.row.applyUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentDate" prop="paymentDate" label="申请日期" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <!-- <el-table-column key="type" prop="type" label="承运属性" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.type? logisticsSearchTypeEnum.VL[scope.row.type]: '-' }}</div>
        </template>
      </el-table-column> -->
      <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="center">
        <template v-slot="scope">
          <div>{{ isNotBlank(scope.row.applyAmount) ? toThousand(scope.row.applyAmount,decimalPrecision.contract): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditStatus" prop="auditStatus" label="状态" align="center">
        <template v-slot="scope">
          <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName? scope.row.auditUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
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
    <detail v-model="detailVisible" :showType="showType" :detailInfo="detailInfo" @success="handleSuccess"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-payment-manage/logistics-payment'
import { ref, defineProps, watch, defineEmits } from 'vue'

import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { auditTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { contractSupplierLogisticsPM } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'
import detail from './detail'

const { decimalPrecision } = useDecimalPrecision()
const permission = contractSupplierLogisticsPM.payment

const optShow = {
  add: true,
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
  }
})

const tableRef = ref()
const detailVisible = ref(false)
const showType = ref('detail')
const detailInfo = ref({})
const emit = defineEmits(['success'])

const { crud } = useCRUD(
  {
    title: '付款申请',
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
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.query.supplierId = props.currentRow.supplierId
      crud.query.branchCompanyId = props.currentRow.branchCompanyId
      crud.query.propertyType = supplierPayTypeEnum.TRANSPORT.V
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function handleSuccess() {
  emit('success')
  crud.toQuery()
}

function openDetail(row, type) {
  detailInfo.value = row
  showType.value = type
  detailVisible.value = true
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

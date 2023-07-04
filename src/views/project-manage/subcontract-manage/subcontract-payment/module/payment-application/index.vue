<template>
  <div>
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;" v-if="checkPermission(permission.add)">添加</common-button>
      <el-tag type="success" size="medium" v-if="detailInfo.amount">合同金额:<span v-thousand="detailInfo.amount" /></el-tag>
      <el-tag type="success" size="medium" v-if="detailInfo?.sourceRow?.settlementAmount" style="margin-left:5px;">结算金额:<span v-thousand="detailInfo?.sourceRow?.settlementAmount" /></el-tag>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="receivingUnit" prop="receivingUnit" label="分包公司" align="center" show-overflow-tooltip />
      <el-table-column key="applyUserName" prop="applyUserName" label="申请人" align="center" show-overflow-tooltip />
      <el-table-column key="paymentDate" prop="paymentDate" label="申请日期" align="center" show-overflow-tooltip />
      <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="right" show-overflow-tooltip />
      <el-table-column key="paymentReasonName" prop="paymentReasonName" label="付款事由" align="center" show-overflow-tooltip />
      <el-table-column key="auditStatus" prop="auditStatus" label="状态" align="center" show-overflow-tooltip >
        <template v-slot="scope">
          <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="actuallyPaymentAmount" prop="actuallyPaymentAmount" label="实付金额" align="right" show-overflow-tooltip />
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" show-overflow-tooltip />
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center" show-overflow-tooltip />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.del])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')" v-permission="permission.detail"/>
          <udOperation :data="scope.row" :show-edit="scope.row.auditStatus===auditTypeEnum.AUDITING.V?true:false" :show-del="scope.row.auditStatus===auditTypeEnum.AUDITING.V?true:false" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm :detail-info="detailInfo" />
    <mDetail :detail-info="detailInfo" v-model="detailVisible" :currentRow="currentRow"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/subcontract-payment/payment-application'
import { ref, defineProps, watch, nextTick, inject, computed } from 'vue'

import { auditTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import { subcontractOrderPaymentPM } from '@/page-permission/project'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './form'
import mDetail from './detail'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const permission = subcontractOrderPaymentPM.paymentManage

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

const dataFormat = computed(() => {
  return [
    ['applyDate', ['parse-time', '{y}-{m}-{d}']],
    ['paymentDate', ['parse-time', '{y}-{m}-{d}']],
    ['auditTime', ['parse-time', '{y}-{m}-{d}']],
    ['applyAmount', ['to-thousand', decimalPrecision.value.project]]
  ]
})

const tableRef = ref()
const orderId = inject('orderId')
const detailVisible = ref(false)
const currentRow = ref({})

const { crud, CRUD } = useCRUD(
  {
    title: '付款申请记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['propertyType', 'orderId'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
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

watch(
  orderId,
  (id) => {
    nextTick(() => {
      crud.query.orderId = id
      crud.query.propertyType = supplierPayTypeEnum.SUBCONTRACT.V
      crud.refresh()
    })
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.applyDate = String(v.applyDate)
  })
}

function openDetail(row, type) {
  currentRow.value = row.sourceRow
  detailVisible.value = true
}
</script>

<style lang="scss" scoped>
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
</style>

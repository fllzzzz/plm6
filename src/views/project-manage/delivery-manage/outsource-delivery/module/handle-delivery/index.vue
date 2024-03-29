<template>
  <div>
    <mHeader :projectId="projectId"/>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="[{id:1}]"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      :stripe="false"
      :showEmptySymbol="false"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="paymentDate" prop="paymentDate" label="单体" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="区域" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="名称" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="编号" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="规格" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="清单数" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="清单量" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="已收" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="本次实收" align="center">
        <template v-slot="scope">
          <el-input-number
            v-model.number="scope.row.visaAmount"
            :min="0"
            :max="scope.row.mete"
            :step="1"
            :precision="0"
            placeholder="本次实收"
            controls-position="right"
            style="width: 100%"
          />
        </template>
      </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <confirmDetail v-model="confirmVisible" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/payment-application'
import { ref, defineProps, watch, defineExpose, computed } from 'vue'

import { supplierMaterialPaymentPM } from '@/page-permission/supply-chain'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './header'
import pagination from '@crud/Pagination'
import confirmDetail from '../confirm-detail'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const permission = supplierMaterialPaymentPM.application

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
  },
  projectId: {
    type: Number,
    default: undefined
  }
})

const dataFormat = computed(() => {
  return [
    ['paymentDate', ['parse-time', '{y}-{m}-{d}']],
    ['auditTime', 'parse-time'],
    ['applyAmount', ['to-thousand', decimalPrecision.value.project]]
  ]
})

const tableRef = ref()
const confirmVisible = ref(false)
const list = computed(() => {
  return crud.data
})

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
      // crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function openConfirm() {
  confirmVisible.value = true
}

// watch(
//   orderId,
//   (id) => {
//     nextTick(() => {
//       crud.query.orderId = id
//       crud.query.propertyType = supplierPayTypeEnum.PURCHASE.V
//       crud.refresh()
//     })
//   },
//   { immediate: true }
// )

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.paymentDate = String(v.paymentDate)
  })
}

defineExpose({
  list,
  openConfirm
})
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

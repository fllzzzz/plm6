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
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="paymentDate" prop="paymentDate" label="付款日期" align="center" width="90" />
      <el-table-column key="applyAmount" prop="applyAmount" label="支付金额" align="right" min-width="80">
        <template #default="{ row }">
          <template v-if="row.attachments && attachments.length>0">
            <div v-for="item in attachments" :key="item.id">
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{toThousand(row.applyAmount)}}</div>
            </div>
          </template>
          <template v-else>{{toThousand(row.applyAmount)}}</template>
        </template>
      </el-table-column>
      <el-table-column key="paymentReasonId" prop="paymentReasonId" label="付款事由" align="center" width="100">
          <template #default="{ row }">
         <div>{{ dict?.label?.['payment_reason']?.[row.paymentReasonId] }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="paymentBank" prop="paymentBank" show-overflow-tooltip label="付款银行" align="center" min-width="130">
        <template #default="{ row }">
          <div>{{row.paymentBank}}{{row.paymentBankAccount?'【'+row.paymentBankAccount+'】':''}}</div>
        </template>
      </el-table-column>
      <el-table-column key="receivingUnit" prop="receivingUnit" label="收款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="receivingBank" prop="receivingBank" label="收款银行" align="center" min-width="140" show-overflow-tooltip>
        <template #default="{ row }">
          <div>{{row.receivingBank}}{{row.receiveBankAccount?'【'+row.receiveBankAccount+'】':''}}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" />
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center" />
      <el-table-column key="auditStatus" prop="auditStatus" label="审核状态" align="center">
        <template v-slot="scope">
          <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.del])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
          <udOperation :data="scope.row" :show-edit="scope.row.auditStatus===auditTypeEnum.AUDITING.V?true:false" :show-del="false" :permission="permission"/>
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
import crudApi from '@/api/contract/supplier-manage/jd-material-payment'
import { ref, defineProps, watch, nextTick, inject } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { supplierMaterialPaymentPM } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './header'
import mForm from './form'
import mDetail from './detail'

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
  }
})

const dataFormat = ref([
  ['paymentDate', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', 'parse-time'],
  ['applyAmount', 'to-thousand']
])

const tableRef = ref()
const supplierId = inject('supplierId')
const detailVisible = ref(false)
const currentRow = ref({})

const { crud, CRUD } = useCRUD(
  {
    title: '付款申请记录',
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
  supplierId,
  (id) => {
    nextTick(() => {
      crud.query.supplierId = id
      crud.refresh()
    })
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.paymentDate = String(v.paymentDate)
  })
}

function openDetail(row, type) {
  currentRow.value = row
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

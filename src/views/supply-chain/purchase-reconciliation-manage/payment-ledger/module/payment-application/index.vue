<template>
  <div>
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;">添加</common-button>
      <el-tag type="success" size="medium" v-if="detailInfo.amount">{{'合同金额:'+toThousand(detailInfo.amount)}}</el-tag>
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
      <el-table-column key="collectionDate" prop="collectionDate" label="*申请日期" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionAmount" prop="collectionAmount" label="申请金额" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="状态" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="审核日期" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
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
          <udOperation :data="scope.row" :show-edit="false" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm :detail-info="detailInfo"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/payment-application'
import { ref, defineProps, watch, nextTick, inject } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
// import { auditTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
// import { DP } from '@/settings/config'
import { toThousand } from '@data-type/number'
import mForm from './form'
import { contractLedgerPM } from '@/page-permission/contract'

const permission = contractLedgerPM.collection

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
const orderId = inject('orderId')

const { crud } = useCRUD(
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

watch(
  orderId,
  (id) => {
    nextTick(() => {
      crud.query.orderId = id
      crud.query.propertyType = 1
      crud.refresh()
    })
  },
  { immediate: true }
)

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

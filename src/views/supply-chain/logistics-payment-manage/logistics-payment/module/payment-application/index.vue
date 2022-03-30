<template>
  <div>
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;" v-if="checkPermission(permission.add)">添加</common-button>
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
       <el-table-column key="applyUserName" prop="applyUserName" label="申请人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName? scope.row.applyUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentDate" prop="paymentDate" label="*付款日期" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.applyAmount && scope.row.applyAmount>0? toThousand(scope.row.applyAmount): scope.row.applyAmount }}</div>
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
        v-if="checkPermission([ ...permission.edit,...permission.del])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :show-edit="scope.row.auditStatus===auditTypeEnum.AUDITING.V?true:false" :show-del="scope.row.auditStatus===auditTypeEnum.AUDITING.V?true:false" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm :detail-info="detailInfo" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-payment-manage/logistics-payment'
import { ref, defineProps, watch, nextTick } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import { auditTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
// import { DP } from '@/settings/config'
import { toThousand } from '@data-type/number'
import mForm from './form'
import { supplierLogisticsPaymentPM } from '@/page-permission/supply-chain'

const permission = supplierLogisticsPaymentPM.application

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

const { CRUD, crud } = useCRUD(
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
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

watch(
  props.detailInfo,
  (id) => {
    nextTick(() => {
      crud.query.supplierId = props.detailInfo.supplierId
      crud.toQuery()
    })
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.paymentDate = String(v.paymentDate)
    return v
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

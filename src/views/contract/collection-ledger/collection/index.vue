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
    style="width: 100%"
    return-source-data
    :showEmptySymbol="false"
    :stripe="false"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" :show-overflow-tooltip="true" label="业务类型" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.businessType?businessTypeEnum.VL[scope.row.businessType]:'废料出售'}}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('relationDept.name')" key="relationDept.name" prop="relationDept.name" :show-overflow-tooltip="true" label="所属部门" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.relationDept?.name || '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('customerUnit')" key="customerUnit" prop="customerUnit" :show-overflow-tooltip="true" label="客户名称" align="left">
      <template v-slot="scope">
        <span>{{ scope.row.customerUnit || '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('project')" key="project.serialNumber" prop="project" :show-overflow-tooltip="true" label="所属项目/购买方" min-width="150">
      <template v-slot="scope">
        <span v-if="scope.row.type === 1">{{ projectNameFormatter(scope.row.project) }}</span>
        <span v-else>{{ scope.row.paymentUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionUnit')" key="collectionUnit" prop="collectionUnit" :show-overflow-tooltip="true" label="签约主体/出售方" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount,decimalPrecision.contract): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionDate')" key="collectionDate" prop="collectionDate" label="收款日期" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionAmount')" key="collectionAmount" prop="collectionAmount" label="收款额(元)" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount,decimalPrecision.contract): scope.row.collectionAmount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionReason')" key="collectionReason" prop="collectionReason" label="收款事由" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi, { getCollectionList } from '@/api/contract/collection-and-invoice/collection'
import { ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { businessTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { projectNameFormatter } from '@/utils/project'
import { collectionLedgerPM } from '@/page-permission/contract'
import useDict from '@compos/store/use-dict'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const { decimalPrecision } = useDecimalPrecision()
const permission = collectionLedgerPM.collection

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dict = useDict(['payment_reason'])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '收款台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getCollectionList },
    invisibleColumns: ['contractAmount'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collectionLedger',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  } else {
    crud.query.startDate = undefined
    crud.query.endDate = undefined
  }
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

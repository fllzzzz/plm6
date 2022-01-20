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
    :stripe="false"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" :show-overflow-tooltip="true" label="业务类型">
      <template v-slot="scope">
        <span>{{ scope.row.businessType?businessTypeEnum.VL[scope.row.businessType]:'-'}}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('project.serialNumber')" key="project.serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="项目">
      <template v-slot="scope">
        <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceDate')" key="invoiceDate" prop="invoiceDate" label="开票日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceDate? parseTime(scope.row.invoiceDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="开票额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount): scope.row.invoiceAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceNo')" key="invoiceNo" prop="invoiceNo" :show-overflow-tooltip="true" label="发票号码" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceNo }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceType')" key="invoiceType" prop="invoiceType" label="发票类型" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceType ? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</span>
        <span v-if="scope.row.taxRate">{{`【${scope.row.taxRate}%】`}}</span>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/collection-and-invoice/invoice'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { invoiceTypeEnum, businessTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { projectNameFormatter } from '@/utils/project'

// crud交由presenter持有
const permission = {
  get: ['invoice:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '开票台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['contractAmount'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.invoice-ledger',
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

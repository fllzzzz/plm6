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
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="项目" min-width="250">
      <template v-slot="scope">
        <template v-if="scope.row.project">
          <div>{{ scope.row.project.serialNumber }}</div>
          <div>{{ scope.row.project.shortName }}</div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)" min-width="150">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? scope.row.contractAmount.toThousand(): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceType')" key="invoiceType" prop="invoiceType" label="发票类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceType ? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="发票面额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? scope.row.invoiceAmount.toThousand(): scope.row.invoiceAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('tax')" key="tax" prop="tax" label="销项税额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <span>{{ scope.row.tax && scope.row.tax>0? scope.row.tax.toThousand(): scope.row.tax }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceDate')" key="invoiceDate" prop="invoiceDate" label="发票日期" align="center" min-width="120">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.invoiceDate }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceNo')" key="invoiceNo" prop="invoiceNo" :show-overflow-tooltip="true" label="发票号码" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceNo }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceUnit')" key="invoiceUnit" prop="invoiceUnit" :show-overflow-tooltip="true" label="开票单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionUnit')" key="collectionUnit" prop="collectionUnit" :show-overflow-tooltip="true" label="收票单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('writtenByName')" key="writtenByName" prop="writtenByName" label="填报人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.writtenByName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="填报日期" align="center" width="110px">
      <template v-slot="scope">
        <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditorName')" key="auditorName" prop="auditorName" label="审核人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditorName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" label="审核日期" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditTime }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('auditStatus')" key="auditStatus" prop="auditStatus" label="状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: ''}}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('source')" key="source" prop="source" label="来源" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.source? systemTypeEnum.VL[scope.row.source]: '' }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.edit,...permission.audit])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')"/>
        <common-button icon="el-icon-s-check" type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V"/>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  <mDetail :collectionInfo="currentInfo" :type="showType"  v-model="detailVisble" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/collection-and-invoice/invoice'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import { auditTypeEnum, systemTypeEnum, invoiceTypeEnum } from '@enum-ms/contract'

// crud交由presenter持有
const permission = {
  get: ['invoice:get'],
  add: ['invoice:add'],
  edit: ['invoice:edit'],
  audit: ['invoice:audit']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentInfo = ref({})
const showType = ref('detail')
const detailVisble = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '开票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['tax', 'invoiceDate', 'invoiceNo', 'createTime', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 157
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisble.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.projectId = v.project.id
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
</style>

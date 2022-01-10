<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType"/>
    </div>
    <!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="[{id:1}]"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left"/>
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="项目类型" min-width="150" fixed="left">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="columns.visible('project.shortName')"
      key="project.shortName"
      prop="project.shortName"
      :show-overflow-tooltip="true"
      label="项目"
      min-width="250"
      fixed="left"
    >
      <template v-slot="scope">
        <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="订单类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="负责人" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectType }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="业务负责人" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="签订日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="合同额" align="center" width="110px">
      <template v-slot="scope">
         <el-tag @click="openContractMoney(scope.row)">合同额</el-tag>
        <!-- <div>{{ scope.row.attachmentCount }}</div> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="结算额" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="累计收款" align="center" width="110px">
      <template v-slot="scope">
        <el-tag @click="openTab(scope.row)">累计收款</el-tag>
        <!-- <div>{{ scope.row.attachmentCount }}</div> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="收款比例" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="累计开票" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="开票比例" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="累计发生额" align="center" width="110px">
      <template v-slot="scope">
        <el-tag @click="openOccurAmount(scope.row)">累计发生额</el-tag>
        <!-- <div>{{ scope.row.attachmentCount }}</div> -->
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="可用余额" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="项目状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
  </common-table>
  <!-- 合同额 -->
  <contract-money v-model="moneyVisible"/>
  <!-- 发生额 -->
  <occur-amount v-model="occurVisible"/>
  <!-- 收付款 -->
  <collectionAndInvoice v-model="tabVisible"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/contract-ledger'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { businessTypeEnum } from '@enum-ms/contract'
import { projectNameFormatter } from '@/utils/project'
import occurAmount from './module/occur-amount'
import contractMoney from './module/contract-money'
import collectionAndInvoice from './module/collection-and-invoice'

const { currentProjectType } = mapGetters(['currentProjectType'])
// crud交由presenter持有
const permission = {
  get: ['contractLedger:get'],
  download: ['contractLedger:download']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const moneyVisible = ref(false)
const occurVisible = ref(false)
const tabVisible = ref(false)
const { crud, columns } = useCRUD(
  {
    title: '项目台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractLedger',
  paginate: true,
  extraHeight: 40
})

function openContractMoney(row) {
  moneyVisible.value = true
}

function openOccurAmount(row) {
  occurVisible.value = true
}

function openTab(row) {
  tabVisible.value = true
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

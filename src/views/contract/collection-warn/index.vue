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
    return-source-data
    :showEmptySymbol="false"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('project')" key="project.shortName" prop="project" :show-overflow-tooltip="true" label="项目" min-width="150">
      <template v-slot="scope">
        <span>{{ projectNameFormatter(scope.row.project) }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('signingDate')" key="signingDate" prop="signingDate" label="签约日期" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.signingDate? parseTime(scope.row.signingDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectManagerName')" key="projectManagerName" prop="projectManagerName" :show-overflow-tooltip="true" label="业务负责人" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.projectManagerName? scope.row.projectManagerName: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" :show-overflow-tooltip="true" label="业务类型" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.businessType?businessTypeEnum.VL[scope.row.businessType]:'-'}}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.contractAmount? toThousand(scope.row.contractAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionAmount')" key="collectionAmount" prop="collectionAmount" label="收款额(元)" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionAmount? toThousand(scope.row.collectionAmount): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('happenedAmount')" key="happenedAmount" prop="happenedAmount" label="发生额(元)" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.happenedAmount? toThousand(scope.row.happenedAmount): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('arrearAmount')" key="arrearAmount" prop="arrearAmount" label="欠款(元)" align="center">
      <template v-slot="scope">
        <el-tag class="collection-tag" :type="scope.row.arrearAmount<0?'warning':'success'" effect="plain">{{ scope.row.arrearAmount && scope.row.arrearAmount>0? toThousand(scope.row.arrearAmount): scope.row.arrearAmount }}</el-tag>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/contract-warn'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { businessTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { projectNameFormatter } from '@/utils/project'
import { collectionWarnPM as permission } from '@/page-permission/contract'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '收款预警',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collectionWarn',
  paginate: true,
  extraHeight: 40
})

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

::v-deep(.collection-tag){
  min-width:120px;
  text-align:right;
}
</style>

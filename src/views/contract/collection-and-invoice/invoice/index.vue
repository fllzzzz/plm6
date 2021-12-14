<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader/>
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
        <div>{{ scope.row.serialNumber }}</div>
        <div>{{ scope.row.shortName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" prop="contractAmount" :show-overflow-tooltip="true" label="合同金额(元)" min-width="150">
      <template v-slot="scope">
        <span class="project-name">{{ scope.row.contractAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="已收款金额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="收款金额(元)" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="收款事由" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="收款方式" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="收款日期" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="收款单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="收款行" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="收款账号" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="付款单位" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="收款行" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="收款账号" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="填报人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="填报日期" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="审核人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="审核日期" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <!-- <el-table-column
      v-if="checkPermission([ ...permission.download])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    > -->
    <el-table-column
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <!-- <udOperation
          :data="scope.row"
          :show-edit="false"
        /> -->
        <!-- 下载 -->
        <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/collection-and-invoice/invoice'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import useDict from '@compos/store/use-dict'
import { projectTypeEnumN, businessTypeEnum } from '@enum-ms/contract'

// crud交由presenter持有
const permission = {
  get: ['invoice:get'],
  download: ['invoice:download']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dict = useDict(['payment_reason'])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '开票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.invoice',
  paginate: true,
  extraHeight: 157
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
</style>

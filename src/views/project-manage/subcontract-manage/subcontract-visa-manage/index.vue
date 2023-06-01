<template>
  <div class="app-container">
    <mHeader />
    <!--表格渲染-->
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
      <el-table-column key="project" prop="project" label="所属项目" align="center" min-width="120"/>
      <el-table-column key="supplierName" prop="supplierName" label="申请单位" align="center" />
      <el-table-column key="subcontractClassName" prop="subcontractClassName" label="分类" align="center" />
      <el-table-column key="applyDate" prop="applyDate" label="申请日期" align="center" />
      <el-table-column key="visaAmount" prop="visaAmount" label="申请签证额" align="right" />
      <el-table-column key="visaReasonName" prop="visaReasonName" label="签证原因" align="center" />
      <el-table-column key="auditStatus" prop="auditStatus" label="状态" align="center">
        <template v-slot="scope">
          <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" />
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center" />
      <el-table-column key="approverName" prop="approverName" label="审批人" align="center" />
      <el-table-column key="approveTime" prop="approveTime" label="审批日期" align="center" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail,...permission.audit])"
        label="操作"
        width="120px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')" v-permission="permission.detail"/>
          <common-button icon="el-icon-s-check" type="primary" size="mini" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.audit)"/>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model="detailVisible" :currentRow="currentRow" :showType="showType" @success="crud.toQuery" :permission="permission"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/subcontract-visa-manage'
import { ref } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'
import { subcontractVisaManagePM as permission } from '@/page-permission/project'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mDetail from './module/detail'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['project', ['parse-project', { onlyShortName: true }]],
  ['applyDate', ['parse-time', '{y}-{m}-{d}']],
  ['visaAmount', ['to-thousand-ck', 'YUAN']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']],
  ['approveTime', ['parse-time', '{y}-{m}-{d}']]
])

const tableRef = ref()
const detailVisible = ref(false)
const currentRow = ref({})
const showType = ref('detail')

const { crud, CRUD } = useCRUD(
  {
    title: '分包签证',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.imgSrc = v.attachments && v.attachments.map((k) => k.imageUrl)
  })
}

function openDetail(row, type) {
  currentRow.value = row
  detailVisible.value = true
  showType.value = type
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

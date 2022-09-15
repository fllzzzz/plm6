<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <mHeader />
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :data-format="dataFormat"
        :stripe="false"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
        <el-table-column key="reportDate" prop="reportDate" label="申报日期" align="center" width="100" v-if="crud.query.yearMonth"/>
        <el-table-column key="supplierName" prop="supplierName" label="分包单位" align="center" />
        <el-table-column key="productType" prop="productType" label="产品分类" align="center" width="150">
          <template v-slot="scope">
            <span>{{installProjectTypeEnum.VL[scope.row.productType]}}</span><span v-if="scope.row.productType===installProjectTypeEnum.ENCLOSURE.V">{{`-${TechnologyTypeAllEnum.VL[scope.row.category]}`}}</span>
          </template>
        </el-table-column>
        <el-table-column key="reportMete" prop="reportMete" label="上报量" align="center">
          <template v-slot="scope">
            <div style="display:flex;">
              <div style="flex:1;"><span>{{scope.row.reportQuantity}}<span style="margin-left:3px;">{{scope.row.measureUnit}}</span></span></div>
              <div style="width:1px;height:50px;background:#ebeef5;position:absolute;left:50%;top:0;"></div>
              <div style="flex:1">{{scope.row.reportMete}}<span style="margin-left:3px;">{{scope.row.accountingUnit}}</span></div>
            </div>
          </template>
        </el-table-column>
        <el-table-column key="actualMete" prop="actualMete" label="审核量" align="center">
          <template v-slot="scope">
            <div style="display:flex;">
              <div style="flex:1;"><span>{{scope.row.actualQuantity}}<span style="margin-left:3px;">{{scope.row.measureUnit}}</span></span></div>
              <div style="width:1px;height:50px;background:#ebeef5;position:absolute;left:50%;top:0;"></div>
              <div style="flex:1">{{scope.row.actualMete}}<span style="margin-left:3px;">{{scope.row.accountingUnit}}</span></div>
            </div>
          </template>
        </el-table-column>
        <el-table-column key="boolAuditStatus" prop="boolAuditStatus" label="状态" align="center" width="100">
          <template v-slot="scope">
            <el-tag :type="scope.row.boolAuditStatus?'success':''">{{installAuditTypeEnum.VL[scope.row.boolAuditStatus]}}</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.detail,...permission.audit])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <common-button
              v-if="checkPermission(permission.detail)"
              size="mini"
              icon="el-icon-view"
              type="primary"
              @click="openDetail(scope.row,'detail')"
            />
            <common-button
              v-if="checkPermission(permission.audit) && !scope.row.boolAuditStatus"
              size="mini"
              icon="el-icon-s-check"
              type="primary"
              @click="openDetail(scope.row,'audit')"
            />
          </template>
        </el-table-column>
      </common-table>
      <pagination />
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
    <installDetail v-model="detailVisible" :showType="showType" :detailInfo="detailInfo" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/install-manage/install-audit'
import { ref, watch } from 'vue'

import { businessTypeEnum, TechnologyTypeAllEnum } from '@enum-ms/contract'
import { installProjectTypeEnum, installAuditTypeEnum } from '@enum-ms/project'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import { installAuditPM as permission } from '@/page-permission/project'
import { mapGetters } from '@/store/lib'

import mHeader from './module/header'
import installDetail from './module/install-detail'
import pagination from '@crud/Pagination'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['reportDate', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']]
  // ['productType', ['parse-enum', installProjectTypeEnum]]
])

const tableRef = ref()
const detailVisible = ref(false)
const showType = ref('detail')
const detailInfo = ref({})
const { crud, CRUD } = useCRUD(
  {
    title: '安装审核',
    sort: ['id.desc'],
    permission: { ...permission },
    requiredQuery: ['projectId'],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.install-audit',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

function openDetail(row, type) {
  detailInfo.value = row.sourceRow
  detailVisible.value = true
  showType.value = type
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content.map(v => {
    v.projectId = crud.query.projectId
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-table .abnormal-row) {
  background: #f0f9eb;
}
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
::v-deep(.el-progress-bar__inner){
  text-align: center;
  max-width: 100%;
}
</style>

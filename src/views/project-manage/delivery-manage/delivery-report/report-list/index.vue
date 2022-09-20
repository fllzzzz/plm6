<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <mHeader :projectId="globalProjectId" />
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :stripe="false"
        :data-format="dataFormat"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" />
        <el-table-column key="name" prop="name" label="名称" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" />
        <el-table-column key="specification" prop="specification" label="规格" align="center" />
        <el-table-column key="measureUnit" prop="measureUnit" label="计量单位" align="center" />
        <el-table-column key="quantity" prop="quantity" label="数量" align="center" />
        <el-table-column key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" />
        <el-table-column key="mete" prop="mete" label="核算量" align="center" />
        <el-table-column key="areaType" prop="areaType" label="属性" align="center" />
        <el-table-column key="supplierName" prop="supplierName" label="供应商" align="center" />
      </common-table>
      <pagination />
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/delivery-manage/delivery-report/report-list'
import { ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { businessTypeEnum } from '@enum-ms/contract'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { deliveryReportListPM as permission } from '@/page-permission/project'
import { mapGetters } from '@/store/lib'

import mHeader from './module/header'
import pagination from '@crud/Pagination'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dataFormat = ref([
  ['areaType', ['parse-enum', manufactureTypeEnum]]
])
const { crud, CRUD } = useCRUD(
  {
    title: '收货报表',
    sort: ['id.desc'],
    permission: { ...permission },
    requiredQuery: ['projectId', 'productType'],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.delivery-report-list',
  paginate: true,
  extraHeight: 40
})
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

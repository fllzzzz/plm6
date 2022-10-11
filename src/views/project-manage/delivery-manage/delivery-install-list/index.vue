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
        style="width: 100%"
      >
        <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer.name" v-if="columns.visible('monomer.name')" :show-overflow-tooltip="true" label="单体" align="center" />
        <el-table-column key="area.name" prop="area.name" v-if="columns.visible('area.name')" :show-overflow-tooltip="true" label="区域" align="center" />
        <el-table-column key="name" prop="name" label="名称" v-if="columns.visible('name')" :show-overflow-tooltip="true" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" v-if="columns.visible('serialNumber')" :show-overflow-tooltip="true" label="编号" align="center" />
        <el-table-column key="material" prop="material" v-if="columns.visible('material')" :show-overflow-tooltip="true" label="材质" align="center" />
        <el-table-column key="specification" prop="specification" v-if="columns.visible('specification')" :show-overflow-tooltip="true" label="规格" align="center" min-width="120"/>
        <el-table-column key="length" prop="length" v-if="columns.visible('length')" :show-overflow-tooltip="true" label="长度(mm)" align="center" />
        <el-table-column key="quantity" prop="quantity" v-if="columns.visible('quantity')" :show-overflow-tooltip="true" label="清单数量" align="center" />
        <el-table-column key="netWeight" prop="netWeight" v-if="columns.visible('netWeight') && crud.query.productType === deliveryInstallTypeEnum.ARTIFACT.V" :show-overflow-tooltip="true" label="单重(kg)" align="center" />
        <el-table-column key="mete" prop="mete" v-if="columns.visible('mete')" :show-overflow-tooltip="true" :label="crud.query.productType === deliveryInstallTypeEnum.ARTIFACT.V?'总重(kg)':'总长度(m)'" align="center" />
        <el-table-column key="receivingQuantity" prop="receivingQuantity" v-if="columns.visible('receivingQuantity')" :show-overflow-tooltip="true" label="已收货" align="center" />
        <el-table-column key="installQuantity" prop="installQuantity" v-if="columns.visible('installQuantity')" :show-overflow-tooltip="true" label="已安装" align="center" />
        <el-table-column key="unAuditQuantity" prop="unAuditQuantity" v-if="columns.visible('unAuditQuantity')" :show-overflow-tooltip="true" label="待审安装数" align="center" />
        <el-table-column key="stockQuantity" prop="stockQuantity" v-if="columns.visible('stockQuantity')" :show-overflow-tooltip="true" label="工地库存" align="center" />
      </common-table>
      <pagination />
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import { deliveryInstallData as get } from '@/api/project-manage/delivery-manage/delivery-report/report-list'
import { ref } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { deliveryInstallTypeEnum } from '@enum-ms/project'
import { deliveryInstallListPM as permission } from '@/page-permission/project'
import { mapGetters } from '@/store/lib'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
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
const { crud, columns, CRUD } = useCRUD(
  {
    title: '收安报表',
    sort: ['id.desc'],
    permission: { ...permission },
    requiredQuery: ['projectId'],
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.delivery-install-list',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.stockQuantity = v.receivingQuantity ? v.receivingQuantity - v.installQuantity - v.unAuditQuantity : 0
    return v
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

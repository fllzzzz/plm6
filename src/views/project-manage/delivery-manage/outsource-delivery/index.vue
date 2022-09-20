<template>
  <div class="app-container">
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
      <el-table-column key="paymentDate" prop="paymentDate" label="供应商" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="商品分类" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="合同量" align="center" />
      <el-table-column key="auditUserName" prop="auditUserName" label="累计收货" align="center" />
      <el-table-column
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button
            v-if="checkPermission(permission.detail)"
            size="mini"
            icon="el-icon-view"
            type="warning"
            @click="openDetail(scope.row,'detail')"
          />
          <common-button
            v-if="checkPermission(permission.detail)"
            size="mini"
            type="primary"
            @click="openHandle(scope.row,'edit')"
          >收货办理</common-button>
        </template>
      </el-table-column>
    </common-table>
    <pagination />
    <common-drawer
      ref="detailRef"
      :show-close="true"
      size="80%"
      title="收货填报详情"
      append-to-body
      v-model="detailVisible"
      :close-on-click-modal="false"
    >
      <template #content>
        <deliveryDetail />
      </template>
    </common-drawer>
    <common-drawer
      ref="drawerRef"
      :show-close="true"
      size="80%"
      title="收货填报"
      append-to-body
      v-model="handleVisible"
      :close-on-click-modal="false"
    >
      <template #titleRight>
        <common-button type="danger" size="mini" @click="handleVisible=false">取消</common-button>
        <common-button type="success" size="mini" @click="handleConfirm">确定</common-button>
      </template>
      <template #content>
        <handleDelivery ref="deliveryConfirm" :projectId="crud.query.projectId"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/sales-manage/visa-manage'
import { ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import { outsourceDeliveryPM as permission } from '@/page-permission/project'

import mHeader from './module/header'
import deliveryDetail from './module/delivery-detail/index'
import handleDelivery from './module/handle-delivery/index'
import pagination from '@crud/Pagination'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailRef = ref()
const drawerRef = ref()
const detailVisible = ref(false)
const handleVisible = ref(false)
const deliveryConfirm = ref()
const { crud } = useCRUD(
  {
    title: '外购收货',
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
  wrapperBox: '.outsource-delivery',
  paginate: true,
  extraHeight: 40
})

function openHandle(row) {
  handleVisible.value = true
}

function openDetail(row, type) {
  detailVisible.value = true
}

function handleConfirm() {
  deliveryConfirm.value.openConfirm()
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

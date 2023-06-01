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
    :data-format="dataFormat"
    return-source-data
    :showEmptySymbol="false"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="crud.query.type===logisticsSearchTypeEnum.PRODUCT.V && columns.visible('projectName')" key="projectName" prop="projectName" show-overflow-tooltip label="所属项目" align="center" min-width="140" />
    <el-table-column v-if="crud.query.type===logisticsSearchTypeEnum.MATERIAL.V && columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" show-overflow-tooltip label="采购合同编号" align="center" min-width="140" />
    <el-table-column v-if="crud.query.type===logisticsSearchTypeEnum.COMPANY.V && columns.visible('supplierName')" key="supplierName" prop="supplierName" show-overflow-tooltip label="物流公司" align="center" min-width="140" />
    <el-table-column v-if="columns.visible('trainNumber')" key="trainNumber" prop="trainNumber" show-overflow-tooltip label="累计使用车次" align="center" min-width="100" />
    <el-table-column v-if="columns.visible('freight')" key="freight" prop="freight" show-overflow-tooltip label="累计运费" align="right" min-width="100" />
    <el-table-column
      v-if="checkPermission([...permission.detail])"
      label="操作"
      width="180"
      align="center"
    >
      <template v-slot="scope">
        <common-button type="primary" icon="el-icon-view" size="mini" @click="openDetail(scope.row)" v-if="checkPermission(permission.detail)"/>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <recordDetail v-model="detailVisible" :detailInfo="detailInfo" :permission="permission" :type="crud.query.type"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { ref } from 'vue'

import { supplierLogisticsLogPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'
import { logisticsSearchTypeEnum } from '@enum-ms/contract'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import recordDetail from './module/record-detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const detailInfo = ref({})
const { crud, columns } = useCRUD(
  {
    title: '物流记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const dataFormat = ref([
  ['freight', ['to-thousand-ck', 'YUAN']]
])

const { maxHeight } = useMaxHeight({
  paginate: true
})

function openDetail(row) {
  detailInfo.value = row
  detailVisible.value = true
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

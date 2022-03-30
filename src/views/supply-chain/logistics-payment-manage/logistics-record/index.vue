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
    return-source-data
    :showEmptySymbol="false"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" :label="crud.query.type===logisticsSearchTypeEnum.PRODUCT.V?'所属项目':(crud.query.type===logisticsSearchTypeEnum.MATERIAL.V?'采购编号':'物流公司')" align="center" min-width="100">
      <template v-slot="scope">
        <div v-if="crud.query.type===logisticsSearchTypeEnum.PRODUCT.V">{{ scope.row.projectName }}</div>
        <div v-if="crud.query.type===logisticsSearchTypeEnum.MATERIAL.V">{{ scope.row.serialNumber }}</div>
        <div v-if="crud.query.type===logisticsSearchTypeEnum.COMPANY.V">{{ scope.row.supplierName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('trainNumber')" key="trainNumber" prop="trainNumber" :show-overflow-tooltip="true" label="累计使用车次" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.trainNumber }}</div>
      </template>
    </el-table-column>
     <el-table-column v-if="columns.visible('freight')" key="freight" prop="freight" :show-overflow-tooltip="true" label="累计运费(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.freight? toThousand(scope.row.freight): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button type="primary" icon="el-icon-view" size="mini" @click="openDetail(scope.row)" v-if="checkPermission(permission.detail)"/>
        </template>
      </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <recordDetail v-model="detailVisible" :detailInfo="detailInfo" :type="crud.query.type"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { ref } from 'vue'
import { supplierLogisticsLogPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { toThousand } from '@data-type/number'
import recordDetail from './module/record-detail'
import { logisticsSearchTypeEnum } from '@enum-ms/contract'

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

const { maxHeight } = useMaxHeight({
  wrapperBox: '.logisticsRecord',
  paginate: true,
  extraHeight: 40
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

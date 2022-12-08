<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <mHeader :projectId="globalProjectId" />
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :data-format="dataFormat"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :stripe="false"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
        <el-table-column key="auditTime" prop="auditTime" label="发运日期" align="center" width="160" :show-overflow-tooltip="true" >
          <template v-slot="scope">
            <table-cell-tag :show="isNotBlank(scope.row.boolProblemReceiving) && scope.row.sourceRow?.status===deliveryStatusEnum.DELIVERY.V" :name="scope.row.boolProblemReceiving?'问题收货':'正常收货'" :color="scope.row.boolProblemReceiving?'#f56c6c':'#67c23a'" @click="openDetail(scope.row,'detail')"/>
            <span style="margin-left: 14px">{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d} {h}:{i}:{s}'): '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="project.shortName"
          prop="project.shortName"
          :show-overflow-tooltip="true"
          label="项目"
          min-width="150"
        >
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="licensePlate" prop="licensePlate" label="车牌号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="serialNumber" prop="serialNumber" label="车次" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="driverName" prop="driverName" label="司机姓名" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="driverPhone" prop="driverPhone" label="司机电话" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="auditUserName" prop="auditUserName" label="办理人" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="productType" prop="productType" label="装载物品" align="center" min-width="120" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <el-tag
              v-for="item in cleanArray(EO.getBits(packTypeEnum, scope.row.productType, 'V'))"
              style="margin-right: 5px"
              :key="item"
              :type="packTypeEnum.V[item].T"
              effect="light"
              disable-transitions
              >{{ packTypeEnum.VL[item] }}</el-tag
            >
          </template>
        </el-table-column>
        <el-table-column key="status" prop="status" label="状态" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="auditReceiptName" prop="auditReceiptName" label="收货人" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="auditReceiptTime" prop="auditReceiptTime" label="收货日期" align="center" min-width="120" :show-overflow-tooltip="true" />
        <el-table-column
          v-if="checkPermission([...permission.detail,...permission.audit])"
          label="操作"
          width="150px"
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
              v-if="checkPermission(permission.audit) && scope.row.sourceRow.status===deliveryStatusEnum.SHIPMENT.V"
              size="mini"
              icon="el-icon-s-check"
              type="primary"
              @click="openDetail(scope.row,'edit')"
            />
          </template>
        </el-table-column>
      </common-table>
      <pagination />
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
    <deliveryDetail v-model="detailVisible" :showType="showType" :detailInfo="detailInfo" @success="crud.toQuery" :permission="permission"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/delivery-manage/homemade-delivery'
import { ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { homemadeDeliveryPM as permission } from '@/page-permission/project'
import { isNotBlank } from '@data-type/index'
import { packTypeEnum } from '@enum-ms/mes'
import { businessTypeEnum } from '@enum-ms/contract'
import { projectNameFormatter } from '@/utils/project'
import { deliveryStatusEnum } from '@enum-ms/project'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import { parseTime } from '@/utils/date'
import { mapGetters } from '@/store/lib'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import deliveryDetail from './module/delivery-detail'
import pagination from '@crud/Pagination'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const showType = ref('detail')
const detailInfo = ref({})
const { crud, CRUD } = useCRUD(
  {
    title: '自制收货',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

const dataFormat = ref([
  ['status', ['parse-enum', deliveryStatusEnum]],
  ['auditReceiptTime', 'parse-time']
])

const { maxHeight } = useMaxHeight({
  wrapperBox: '.homemade-delivery',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  detailVisible.value = true
  detailInfo.value = row.sourceRow
  showType.value = type
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

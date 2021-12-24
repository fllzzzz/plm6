<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('project.shortName') && !crud.query.projectId"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('manufactureType')"
        key="manufactureType"
        prop="manufactureType"
        :show-overflow-tooltip="true"
        label="制造类型"
        align="center"
        min-width="80"
      >
        <template v-slot="scope">
          <el-tag :type="manufactureTypeEnum.V[scope.row.manufactureType].T" effect="plain" disable-transitions>{{
            manufactureTypeEnum.VL[scope.row.manufactureType]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        label="装载类型"
        width="165"
      >
        <template v-slot="scope">
          <el-tag v-for="item in cleanArray(EO.getBits(packTypeEnum, scope.row.productType, 'V'))" style="margin-right:5px;" :key="item" :type="packTypeEnum.V[item].T" effect="light" disable-transitions>{{
            packTypeEnum.VL[item]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <el-tag effect="light" disable-transitions style="width: 100%; max-width: 130px">{{ scope.row.serialNumber }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('receiptStatus')"
        key="receiptStatus"
        prop="receiptStatus"
        sortable="custom"
        label="收货状态"
        align="center"
        min-width="90"
      >
        <template v-slot="scope">
          <el-tag :type="receiptStatusEnum.V[scope.row.receiptStatus].T" disable-transitions effect="plain">{{
            receiptStatusEnum.VL[scope.row.receiptStatus]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        prop="licensePlate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="车牌号"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('driverName')"
        key="driverName"
        prop="driverName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="司机姓名"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('driverPhone')"
        key="driverPhone"
        prop="driverPhone"
        :show-overflow-tooltip="true"
        label="司机电话"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('auditUserName')"
        key="auditUserName"
        prop="auditUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="发运人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('receiptName')"
        key="receiptName"
        prop="receiptName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="收货人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('auditReceiptName')"
        key="auditReceiptName"
        prop="auditReceiptName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="实际收货人"
        align="center"
        min-width="100"
      />
      <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" sortable="custom" label="发运日期" width="120">
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.auditTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditReceiptTime')"
        key="auditReceiptTime"
        prop="auditReceiptTime"
        sortable="custom"
        label="收货日期"
        width="120"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.auditReceiptTime }}</span>
        </template>
      </el-table-column>
      <!--详情与下载-->
      <el-table-column
        v-if="checkPermission([...permission.download, ...permission.detail, ...permission.detailPrint])"
        label="操作"
        width="120px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <!-- 详情 -->
          <common-button type="primary" icon="el-icon-view" size="mini" @click.stop="showDetail(scope.row)" />
          <!-- 下载 -->
          <e-operation :data="{ ...scope.row }" :permission="permission.download" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-detail v-model:visible="detailVisible" :detail-info="receiptInfo" title="装车详情" :detailFunc="detail">
      <template #tip>
        <el-tag effect="plain" size="medium" style="margin-left: 5px" type="danger">车次：{{ receiptInfo.serialNumber }}</el-tag>
        <el-tag effect="plain" size="medium">项目：{{ receiptInfo.project && receiptInfo.project.shortName }}</el-tag>
        <el-tag effect="plain" size="medium" type="success">发运人：{{ receiptInfo.auditUserName }}</el-tag>
        <el-tag effect="plain" size="medium" type="success">收货人：{{ receiptInfo.receiptName }}</el-tag>
      </template>
    </m-detail>
  </div>
</template>

<script setup>
import crudApi, { detail } from '@/api/mes/pack-and-ship/receipt-status'
import { ref } from 'vue'

import { manufactureTypeEnum } from '@enum-ms/production'
import { packTypeEnum, receiptStatusEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import mHeader from './module/header'
import mDetail from '../components/common-detail'

const permission = {
  get: ['receiptStatus:get'],
  detail: ['receiptStatus:detail'],
  print: ['receiptStatus:print'],
  detailPrint: ['receiptStatus:detailPrint'],
  download: ['receiptStatus:download'],
  downloadLogistics: ['receiptStatus:downloadAll']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '收货状态',
    sort: ['auditReceiptTime.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    invisibleColumns: ['manufactureType', 'productType']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const receiptInfo = ref({})

function showDetail(row) {
  receiptInfo.value = row
  detailVisible.value = true
}
</script>

<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      row-key="id"
    >
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left"/>
      <el-table-column
        v-if="columns.visible('qualityTestingEnum')"
        key="qualityTestingEnum"
        :show-overflow-tooltip="true"
        prop="qualityTestingEnum"
        label="状态"
        align="center"
        width="80"
        fixed="left"
      >
        <template #default="{ row }">
          <el-tag :type="inspectionDetailStatusEnum.V[row.qualityTestingEnum].TAG">
            {{ inspectionDetailStatusEnum.V[row.qualityTestingEnum].L }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="申请时间"
        align="center"
        width="140"
      />
      <!-- 基础信息 -->
      <material-base-info-columns :showIndex="false" :basic-class="crud.query.basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :basic-class="crud.query.basicClass" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :basic-class="crud.query.basicClass" />
      <warehouse-info-columns show-project/>
      <el-table-column
        v-if="columns.visible('purchaseSN')"
        key="purchaseSN"
        :show-overflow-tooltip="true"
        prop="purchaseSN"
        label="采购单号"
        min-width="155"
      >
        <template #default="{ row }">
          <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" :offset="10" />
          {{ row.purchaseSN }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        min-width="160"
        label="入库单号"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        :show-overflow-tooltip="true"
        prop="licensePlate"
        label="车牌号"
        align="left"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('shipmentNumber')"
        key="shipmentNumber"
        prop="shipmentNumber"
        label="物流单号"
        align="left"
        min-width="150"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="供应商"
        min-width="200"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/quality-inspection-manage/inbound-inspection-detail'
import { inboundInspectionDetailPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { inspectionDetailStatusEnum } from '@enum-ms/wms'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import mHeader from './module/header'
import pagination from '@crud/Pagination'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...wmsReceiptColumns, ['project', ['parse-project', { onlyShortName: true }]], ['approvalComments', 'empty-text']])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '入库质检明细',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime', 'licensePlate', 'shipmentNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
  data.content = data.content.map(v => {
    v.createTime = v.inboundReceipt?.createTime
    v.supplier = v.inboundReceipt?.supplier
    v.licensePlate = v.inboundReceipt?.licensePlate
    v.shipmentNumber = v.inboundReceipt?.shipmentNumber
    v.purchaseSN = v.inboundReceipt?.purchaseSN
    v.spec = v.specification
    return v
  })
}
</script>

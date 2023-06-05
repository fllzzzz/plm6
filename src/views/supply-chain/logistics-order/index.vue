<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="!loaded || crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
          <p>
            备注：<span>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="日期"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="crud.query && crud.query.logisticsTransportType !== logisticsTransportTypeEnum.FREIGHT.V && columns.visible('shipmentNumber')"
        key="shipmentNumber"
        prop="shipmentNumber"
        label="物流单号"
        align="left"
        min-width="150"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="crud.query && crud.query.logisticsTransportType !== logisticsTransportTypeEnum.POST.V && columns.visible('licensePlate')"
        key="licensePlate"
        prop="licensePlate"
        label="车牌号"
        align="left"
        min-width="100"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('loadingWeight')"
        key="loadingWeight"
        :show-overflow-tooltip="true"
        prop="loadingWeight"
        label="装载重量(kg)"
        min-width="120"
        align="right"
      />
      <el-table-column
        v-if="columns.visible('invoiceType')"
        key="invoiceType"
        :show-overflow-tooltip="true"
        prop="invoiceType"
        label="票据类型"
        align="center"
        min-width="130"
      />
      <el-table-column
        v-if="columns.visible('taxRate')"
        key="taxRate"
        :show-overflow-tooltip="true"
        prop="taxRate"
        label="税率"
        align="right"
        width="70"
      />
      <el-table-column
        v-if="columns.visible('freight')"
        key="freight"
        :show-overflow-tooltip="true"
        prop="freight"
        label="运费(含税)"
        align="right"
        min-width="90"
      />
      <el-table-column
        v-if="columns.visible('amountExcludingVAT')"
        key="amountExcludingVAT"
        :show-overflow-tooltip="true"
        prop="amountExcludingVAT"
        label="不含税金额"
        min-width="90"
        align="right"
      />
      <el-table-column
        v-if="columns.visible('inputVAT')"
        key="inputVAT"
        :show-overflow-tooltip="true"
        prop="inputVAT"
        label="进项税额"
        align="right"
        min-width="80"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="物流单号(系统)"
        align="center"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('purchaseSN')"
        key="purchaseSN"
        :show-overflow-tooltip="true"
        prop="purchaseSN"
        label="采购合同编号"
        align="center"
        min-width="170"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['PURCHASE']" :receipt="row.purchaseOrder" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundSN')"
        key="inboundSN"
        :show-overflow-tooltip="true"
        prop="inboundSN"
        label="入库单号"
        align="center"
        min-width="170"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['INBOUND']" :receipt="row.inboundReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="物流公司"
        min-width="200"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-order'
import { logisticsOrderPM as permission } from '@/page-permission/supply-chain'

import { ref, computed } from 'vue'
import EO from '@enum'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { baseMaterialTypeEnum } from '@enum-ms/wms'
import { matClsEnum } from '@enum-ms/classification'
import { logisticsTransportTypeEnum } from '@enum-ms/logistics'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useSuppliers from '@compos/store/use-suppliers'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ...wmsReceiptColumns,
  ['loadingWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['invoiceType', ['parse-enum', invoiceTypeEnum]],
  ['taxRate', ['suffix', '%']],
  ['inputVAT', ['to-thousand-ck', 'YUAN']],
  ['amountExcludingVAT', ['to-thousand-ck', 'YUAN']],
  ['freight', ['to-thousand-ck', 'YUAN']],
  ['amount', ['to-thousand-ck', 'YUAN']],
  ['requisitionsSNStr', 'empty-text'],
  ['remark', 'empty-text'],
  ['auxMaterialNames', 'split']
])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '物料物流订单',
    sort: ['id.desc'],
    invisibleColumns: ['projects', 'purchaseSN', 'serialNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const expandRowKeys = ref([])
const { loaded, supplierKV } = useSuppliers()
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.materialTypeText = baseMaterialTypeEnum.VL[v.purchaseType] + ' - ' + basicClassArr.join(' | ')
    v.supplier = computed(() => supplierKV.value[v.supplierId])
    return v
  })
}
</script>

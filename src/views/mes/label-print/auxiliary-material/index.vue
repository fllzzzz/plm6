<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader ref="headRef" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="id"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="名称" align="center">
        <template #default="{ row }">
          <table-cell-tag :show="Boolean(row.printedQuantity)" name="已打印" color="#e64242" :offset="15" />
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('measureUnit')"
        key="measureUnit"
        prop="measureUnit"
        :show-overflow-tooltip="true"
        align="center"
        label="单位"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        :show-overflow-tooltip="true"
        align="center"
        label="规格"
      />
      <el-table-column
        v-if="columns.visible('useProperty')"
        key="useProperty"
        prop="useProperty"
        :show-overflow-tooltip="true"
        align="center"
        label="使用范围"
      >
        <template #default="{ row }">
          <span>{{ typeEnum.VL[row.useProperty] }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" label="数量" align="center" />
      <!-- <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        align="center"
        label="备注"
      /> -->
      <!-- <belonging-info-columns :columns="columns" showMonomer /> -->
      <!-- <productType-base-info-columns :productType="productType" :columns="columns">
        <template #namePrefix="{ row }">
          <table-cell-tag :show="Boolean(row.printedQuantity)" name="已打印" color="#e64242" :offset="15" />
        </template>
      </productType-base-info-columns> -->
      <!-- <el-table-column
        v-if="columns.visible('measureUnit')"
        :show-overflow-tooltip="true"
        prop="measureUnit"
        label="计量单位"
        align="center"
        min-width="70px"
      >
        <template #default="{ row }">
          <span>{{ row.measureUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        :show-overflow-tooltip="true"
        prop="quantity"
        label="数量"
        align="center"
        min-width="120px"
      >
        <template #default="{ row }">
          <span>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('accountingUnit')"
        :show-overflow-tooltip="true"
        prop="accountingUnit"
        label="核算单位"
        align="center"
        min-width="70px"
      >
        <template #default="{ row }">
          <span>{{ row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('mete')"
        :show-overflow-tooltip="true"
        prop="mete"
        label="核算量"
        align="center"
        min-width="120px"
      > -->
      <!-- <template #default="{ row }">
          <span>{{ row.mete }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('printQuantity')"
        key="printQuantity"
        prop="printQuantity"
        sortable="custom"
        label="打印数量"
        align="center"
        width="120px"
      >
        <template #default="{ row: { sourceRow: row } }">
          <el-input-number v-model="row.printQuantity" :step="1" :min="0" size="mini" style="width: 100%" controls-position="right" />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="225px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <common-button icon="el-icon-printer" type="success" size="mini" @click="printLabel(row)" />
          <common-button icon="el-icon-view" type="primary" size="mini" @click="previewLabel(row)" />
          <common-button type="info" size="mini" @click="openRecordView(row)">打印记录</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <printed-record-drawer v-model:visible="recordVisible" :task-id="currentMId" :getPrintRecord="getPrintRecord" />
    <label-dlg v-model:visible="labelVisible" :label-data="currentLabel" :productType="productType" :labelType="labelType" />
  </div>
</template>

<script setup>
import { getForMaterial as getPrintRecord } from '@/api/mes/label-print/print-record'
import crudApi from '@/api/mes/label-print/auxiliary-material'
import { ref, provide } from 'vue'

import { componentTypeEnum, labelTypeEnum } from '@enum-ms/mes'
import { auxiliaryMaterialUseTypeEnum as typeEnum } from '@enum-ms/plan'
import { QR_SCAN_F_TYPE } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { printAuxiliaryMaterial as printComponent } from '@/utils/print/index'
import { auxiliaryMaterialLabelPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
// import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
// import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import mHeader from './module/header.vue'
import labelDlg from '../components/label-dlg'
import printedRecordDrawer from '../components/task-printed-record-drawer.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const headRef = ref()
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '产品标签-配套件',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['projectId'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const labelVisible = ref(false)
const currentLabel = ref({})
const currentMId = ref()
const recordVisible = ref(false)
const productType = componentTypeEnum.AUXILIARY_MATERIAL.V
provide('productType', productType)
const labelType = labelTypeEnum.COMMON.V // 配套件默认一种

async function printLabel(row) {
  try {
    await headRef.value.print([row])
  } catch (error) {
    console.log('打印标签失败', error)
  }
}

function previewLabel(row) {
  console.log(row, 'row')
  currentLabel.value = getLabelInfo(row)
  labelVisible.value = true
}

function openRecordView(row) {
  currentMId.value = row.id
  recordVisible.value = true
}

function getLabelInfo(row) {
  const { printConfig, spliceQrCodeUrl, QR_SCAN_PATH, requestUrl, companyName } = headRef.value
  // 标签构件信息
  const component = {
    projectName: row.project?.shortName,
    printTime: row.printTime ? parseTime(row.printTime, '{y}/{m}/{d}') : parseTime(new Date().getTime(), '{y}/{m}/{d}'),
    monomerName: row.monomer?.name,
    areaName: row.area?.name,
    name: row.name,
    // serialNumber: row.serialNumber,
    unit: row.measureUnit,
    useProperty: typeEnum.VL[row.useProperty],
    quantity: row.quantity,
    specification: row.specification
    // drawingNumber: row.drawingNumber
  }
  const baseUrl = requestUrl
  return {
    productType,
    labelType,
    component,
    printConfig,
    manufacturerName: printConfig.manufacturerName || companyName,
    qrCode: spliceQrCodeUrl(`${baseUrl}${QR_SCAN_PATH.AUXILIARY_MATERIAL}`, {
      id: row.id, // id
      ftype: QR_SCAN_F_TYPE.MEW_PRODUCTION,
      // factoryId: row.factoryId, // 工厂id
      // taskId: row.taskId, // 任务id
      type: productType // 类型
      // wt: printConfig.weight, // 重量类型
      // mn: printConfig.manufacturerName, // 制造商名称
      // sl: Number(printConfig.showProductionLine), // 显示生产线
      // sa: Number(printConfig.showArea), // 显示区域
      // sm: Number(printConfig.showMonomer) // 显示单体
    })
  }
}

provide('permission', permission)
provide('headerObj', {
  getLabelInfo,
  printLabelFunc: printComponent
})
</script>

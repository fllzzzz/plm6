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
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120px"
      >
        <template v-slot="scope">
          <table-cell-tag :show="Boolean(scope.row.printedQuantity)" name="已打印" color="#e64242" :offset="15" />
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('color')"
        key="color"
        prop="color"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="颜色"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.color }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单长\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`板厚\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('width')"
        key="width"
        prop="width"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`有效宽度\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        sortable="custom"
        :label="`总面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总长度\n(m)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.totalLength, DP.MES_ENCLOSURE_L__M) }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('weight')"
        key="weight"
        prop="weight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`重量\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ toFixed(scope.row.weight, DP.COM_WT__KG) }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.remark }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        label="数量"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('printQuantity')"
        key="printQuantity"
        prop="printQuantity"
        sortable="custom"
        label="打印数量"
        align="center"
        width="120px"
      >
        <template v-slot="scope">
          <el-input-number v-model="scope.row.printQuantity" :step="1" :min="0" size="mini" style="width: 100%" controls-position="right" />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="225px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button icon="el-icon-printer" type="success" size="mini" @click="printLabel(scope.row)" />
          <common-button icon="el-icon-view" type="primary" size="mini" @click="previewLabel(scope.row)" />
          <common-button type="info" size="mini" @click="openRecordView(scope.row)">打印记录</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <printed-record-drawer v-model:visible="recordVisible" :task-id="currentTaskId" />
    <label-dlg v-model:visible="labelVisible" :label-data="currentLabel"  :productType="productType" :labelType="labelType"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/label-print/enclosure'
import { ref, provide, computed } from 'vue'

import { componentTypeEnum, mesEnclosureTypeEnum, printProductTypeEnum } from '@enum-ms/mes'
import { DP, QR_SCAN_F_TYPE } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { parseTime } from '@/utils/date'
import { printEnclosure as printComponent } from '@/utils/print/index'
import { enclosureLabelPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import tableCellTag from '@comp-common/table-cell-tag/index'
import mHeader from '../components/label-print-header.vue'
import printedRecordDrawer from '../components/task-printed-record-drawer.vue'
import labelDlg from '../components/label-dlg'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const headRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '产品标签-围护',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaId', 'productionLineId'],
    queryOnPresenterCreated: false,
    invisibleColumns: ['remark']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.beforeToQuery = () => {
  crud.query.category = mesEnclosureTypeEnum.FOLDING_PIECE.V
}

const labelVisible = ref(false)
const currentLabel = ref({})
const currentTaskId = ref()
const recordVisible = ref(false)
const productType = componentTypeEnum.ENCLOSURE.V
provide('productType', productType)
const printType = printProductTypeEnum.ENCLOSURE.V
provide('printType', printType)
const labelType = computed(() => {
  return headRef.value?.printConfig?.type
})

async function printLabel(row) {
  try {
    await headRef.value.print([row])
  } catch (error) {
    console.log('打印标签失败', error)
  }
}

function previewLabel(row) {
  currentLabel.value = getLabelInfo(row)
  labelVisible.value = true
}

function openRecordView(row) {
  currentTaskId.value = row.taskId
  recordVisible.value = true
}

function getLabelInfo(row) {
  const { printConfig, spliceQrCodeUrl, QR_SCAN_PATH, requestUrl, companyName } = headRef.value
  // 标签构件信息
  const component = {
    projectName: row.project.shortName,
    printTime: row.printTime ? parseTime(row.printTime, '{y}/{m}/{d}') : parseTime(new Date().getTime(), '{y}/{m}/{d}'),
    monomerName: row.monomer.name,
    areaName: row.area.name,
    name: row.name,
    serialNumber: row.serialNumber,
    color: row.color,
    plate: row.plate,
    thickness: row.thickness && row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM),
    length: row.length,
    quantity: row.quantity,
    specification: row.specification,
    drawingNumber: row.drawingNumber
  }
  // 生产线信息
  // const productionLine = getLine()
  const baseUrl = requestUrl
  return {
    productType,
    labelType: labelType.value,
    component,
    printConfig,
    // productionLineName: printConfig.showProductionLine ? `${productionLine.factoryName}-${productionLine.name}` : '',
    manufacturerName: printConfig.manufacturerName || companyName,
    qrCode: spliceQrCodeUrl(`${baseUrl}/#${QR_SCAN_PATH.ARTIFACT_TASK}`, {
      id: row.id, // id
      ftype: QR_SCAN_F_TYPE.MEW_PRODUCTION,
      factoryId: row.factoryId, // 工厂id
      taskId: row.taskId, // 任务id
      type: productType, // 类型
      wt: printConfig.weight, // 重量类型
      mn: printConfig.manufacturerName, // 制造商名称
      sl: Number(printConfig.showProductionLine), // 显示生产线
      sa: Number(printConfig.showArea), // 显示区域
      sm: Number(printConfig.showMonomer) // 显示单体
    })
  }
}

provide('headerObj', {
  getLabelInfo,
  printLabelFunc: printComponent
})
</script>

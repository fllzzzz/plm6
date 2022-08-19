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
      <el-table-column type="selection" width="55" align="center" :selectable="selectable" />
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
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`长度\n(mm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="80px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('netWeight')"
        key="netWeight"
        prop="netWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单净重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.netWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossWeight')"
        key="grossWeight"
        prop="grossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单毛重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.grossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        key="totalNetWeight"
        prop="totalNetWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总净重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalNetWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalGrossWeight')"
        key="totalGrossWeight"
        prop="totalGrossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总毛重\n(kg)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalGrossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('drawingNumber')"
        key="drawingNumber"
        prop="drawingNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="图号"
        min-width="140px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.drawingNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('surfaceArea')"
        key="surfaceArea"
        prop="surfaceArea"
        sortable="custom"
        :label="`面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.surfaceArea, DP.COM_AREA__M2) }}
        </template>
      </el-table-column>
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
          <el-input-number
            v-model="scope.row.printQuantity"
            :step="1"
            :min="0"
            size="mini"
            :disabled="scope.row.boolOneCode"
            style="width: 100%"
            controls-position="right"
          />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="225px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button icon="el-icon-printer" type="success" size="mini" @click="beforePrintLabel(scope.row)" />
          <common-button icon="el-icon-view" type="primary" size="mini" @click="previewLabel(scope.row)" />
          <common-button type="info" size="mini" @click="openRecordView(scope.row)">打印记录</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <printed-record-drawer v-model:visible="recordVisible" :task-id="currentTaskId" :getPrintRecord="getPrintRecord" />
    <label-dlg v-model:visible="labelVisible" :label-data="currentLabel" :productType="productType" :labelType="labelType">
      <template #oneCode v-if="curNumberList.length">
        <one-code-number-list style="margin-bottom:15px;" v-model="previewCode" :list="curNumberList" :tagWidth="50" :multiple="false" :max-height="80"/>
      </template>
    </label-dlg>
    <!-- 一物一码 选择弹窗 -->
    <common-dialog title="选择一物一码编号" v-model="oneCodeVisible" :center="false" :close-on-click-modal="false" width="450px">
      <template #titleRight>
        <common-button type="primary" size="mini" @click="oneCodeSave">确认</common-button>
      </template>
      <one-code-number-list v-model="curRowSelect" :list="curNumberList" />
    </common-dialog>
  </div>
</template>

<script setup>
import { getForTask as getPrintRecord } from '@/api/mes/label-print/print-record'
import crudApi from '@/api/mes/label-print/artifact'
import { ref, provide, computed, watch } from 'vue'
import { ElMessage } from 'element-plus'

import { weightTypeEnum as printWeightTypeEnum } from '@enum-ms/common'
import { componentTypeEnum, printProductTypeEnum } from '@enum-ms/mes'
import { DP, QR_SCAN_F_TYPE } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { parseTime } from '@/utils/date'
import { printArtifact as printComponent } from '@/utils/print/index'
import { artifactLabelPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import tableCellTag from '@comp-common/table-cell-tag/index'
import mHeader from '../components/label-print-header.vue'
import printedRecordDrawer from '../components/task-printed-record-drawer.vue'
import labelDlg from '../components/label-dlg'
import oneCodeNumberList from '@/components-system/mes/one-code-number-list'

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
    title: '产品标签-构件',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaId', 'productionLineId'],
    queryOnPresenterCreated: false,
    invisibleColumns: ['totalNetWeight', 'totalGrossWeight', 'drawingNumber', 'surfaceArea', 'remark']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const labelVisible = ref(false)
const currentLabel = ref({})
const currentTaskId = ref()
const recordVisible = ref(false)
const productType = componentTypeEnum.ARTIFACT.V
provide('productType', productType)
const printType = printProductTypeEnum.ARTIFACT.V
provide('printType', printType)
const labelType = computed(() => {
  return headRef.value?.printConfig?.type
})

const curRow = ref()
const oneCodeVisible = ref(false)
const saveOneCodeData = ref()
const curNumberList = ref([])
const curRowSelect = ref([])
const previewCode = ref()

function selectable(row, rowIndex) {
  return !row.boolOneCode
}

function getNumberList(quantity) {
  const _numArr = []
  for (let i = 0; i < quantity; i++) {
    _numArr.push({ number: i + 1 })
  }
  return _numArr
}

function beforePrintLabel(row) {
  if (row.boolOneCode) {
    curNumberList.value = getNumberList(row.quantity)
    saveOneCodeData.value = { row }
    oneCodeVisible.value = true
  } else {
    printLabel(row)
  }
}

async function oneCodeSave() {
  if (!curRowSelect.value?.length) {
    ElMessage.warning('请选择需要打印的一物一码编号')
    return
  }
  try {
    const { row } = saveOneCodeData.value
    row.numberList = curRowSelect.value
    row.printQuantity = curRowSelect.value.length
    await printLabel(row)
  } catch (error) {
    console.log('一物一码打印标签失败', error)
  } finally {
    oneCodeVisible.value = false
  }
}

async function printLabel(row) {
  try {
    await headRef.value.print([row])
  } catch (error) {
    console.log('打印标签失败', error)
  }
}

watch(
  () => previewCode.value,
  () => {
    if (labelVisible.value) {
      currentLabel.value = getLabelInfo(curRow.value, previewCode.value)
    }
  }
)

function previewLabel(row) {
  curRow.value = row
  if (row.boolOneCode) {
    curNumberList.value = getNumberList(row.quantity)
    previewCode.value = 1
    currentLabel.value = getLabelInfo(row, previewCode.value)
  } else {
    curNumberList.value = []
    currentLabel.value = getLabelInfo(row)
  }
  labelVisible.value = true
}

function openRecordView(row) {
  currentTaskId.value = row.taskId
  recordVisible.value = true
}

function getLabelInfo(row, num) {
  // const { getLine, printConfig, spliceQrCodeUrl, QR_SCAN_PATH, requestUrl, companyName } = headRef.value
  const { printConfig, spliceQrCodeUrl, QR_SCAN_PATH, requestUrl, companyName } = headRef.value
  // 标签构件信息
  const component = {
    projectName: row.project.shortName,
    printTime: row.printTime ? parseTime(row.printTime, '{y}/{m}/{d}') : parseTime(new Date().getTime(), '{y}/{m}/{d}'),
    monomerName: row.monomer.name,
    areaName: row.area.name,
    name: row.name,
    serialNumber: row.serialNumber,
    quantity: row.quantity,
    specification: row.specification,
    drawingNumber: row.drawingNumber,
    weight:
      printConfig.weight === printWeightTypeEnum.NET.V ? row.netWeight.toFixed(DP.COM_WT__KG) : row.grossWeight.toFixed(DP.COM_WT__KG),
    length: row.length
  }
  // 生产线信息
  // const productionLine = getLine()
  const baseUrl = requestUrl
  const qrCodeObj = {
    id: row.id, // id
    ftype: QR_SCAN_F_TYPE.MEW_PRODUCTION,
    factoryId: row.factoryId, // 工厂id
    taskId: row.taskId, // 任务id
    type: productType, // 类型
    wt: printConfig.weight, // 重量类型
    // sl: Number(printConfig.showProductionLine), // 显示生产线
    sa: Number(printConfig.showArea), // 显示区域
    sm: Number(printConfig.showMonomer) // 显示单体
  }
  if (row.boolOneCode) {
    qrCodeObj.num = num
  }
  return {
    productType,
    labelType: labelType.value,
    component,
    printConfig,
    manufacturerName: printConfig.manufacturerName || companyName,
    qrCode: spliceQrCodeUrl(`${baseUrl}${QR_SCAN_PATH.ARTIFACT_TASK}`, qrCodeObj)
  }
}

provide('headerObj', {
  getLabelInfo,
  printLabelFunc: printComponent
})
</script>

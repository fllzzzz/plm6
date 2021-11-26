<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader ref="headRef">
        <template v-slot:customSearch>
          <el-input
            v-model="crud.query.name"
            size="small"
            placeholder="输入名称搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="crud.query.serialNumber"
            size="small"
            placeholder="输入编号搜索"
            style="width: 170px"
            class="filter-item"
            clearable
            @keyup.enter="crud.toQuery"
          />
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
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
      />
      <el-table-column
        v-if="columns.visible('color')"
        key="color"
        prop="color"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="颜色"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="120px"
      />
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
      <el-table-column
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
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="120px"
      />
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
    <label-dlg v-model:visible="labelVisible" :label-data="currentLabel" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/label-print/enclosure'
import { ref, provide } from 'vue'

import { weightTypeEnum as printWeightTypeEnum } from '@enum-ms/common'
import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { printArtifact as printComponent } from '@/utils/print/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import tableCellTag from '@comp-common/table-cell-tag/index'
import mHeader from '../components/label-print-header.vue'
import printedRecordDrawer from '../components/task-printed-record-drawer.vue'
import labelDlg from './module/label-dlg'

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

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
    title: '产品标签-折边件',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
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
  console.log(headRef.value)
  const { getLine, printConfig, spliceQrCodeUrl, QR_SCAN_PATH, requestUrl } = headRef.value
  // 标签构件信息
  const component = {
    projectName: row.project.shortName,
    monomerName: printConfig.showMonomer ? row.monomer.name : '',
    areaName: printConfig.showArea ? row.district.name : '',
    name: row.name,
    serialNumber: row.serialNumber,
    quantity: row.quantity,
    specification: row.specification,
    drawingNumber: row.drawingNumber,
    weight: printWeightTypeEnum.NET.V ? row.netWeight.toFixed(DP.COM_WT__KG) : row.grossWeight.toFixed(DP.COM_WT__KG),
    length: convertUnits(row.length, 'mm', 'm', DP.MES_ARTIFACT_L__M)
  }
  // 生产线信息
  const productionLine = getLine()
  const baseUrl = requestUrl
  return {
    component,
    productionLineName: printConfig.showProductionLine ? `${productionLine.factoryName}-${productionLine.name}` : '',
    manufacturerName: printConfig.manufacturerName,
    qrCode: spliceQrCodeUrl(`${baseUrl}/#${QR_SCAN_PATH.ARTIFACT_TASK}`, {
      id: row.id, // id
      factoryId: productionLine.factoryId, // 工厂id
      taskId: row.taskId, // 任务id
      type: componentTypeEnum.ARTIFACT.V, // 类型
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

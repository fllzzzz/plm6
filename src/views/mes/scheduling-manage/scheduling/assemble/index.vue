<template>
  <div class="app-container">
    <mHeader v-model:modifying="modifying" v-model:lines="lines" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="productFormat[productType]"
      :stripe="false"
      return-source-data
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :row-class-name="handleRowClassName"
      :cell-class-name="handelCellClassName"
      style="width: 100%"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed :selectable="selectable" />
      <el-table-column label="序号" type="index" align="center" width="60" fixed />
      <el-table-column
        v-if="columns.visible('areaName')"
        key="areaName"
        prop="areaName"
        fixed
        sortable="custom"
        :show-overflow-tooltip="true"
        label="区域"
        width="120px"
      />
      <productType-full-info-columns
        :productType="productType"
        snClickable
        @drawingPreview="drawingPreview"
        :columns="columns"
        :fixed="'left'"
        fixedWidth
      />
      <template v-for="workshop in lines">
        <template v-for="line in workshop.productionLineList">
          <el-table-column
            v-if="line.selected"
            :key="line.id"
            :prop="`productionLine_${line.id}`"
            :label="line.name"
            align="center"
            width="150px"
          >
            <template v-slot:header>
              <span class="ellipsis-text">【{{ workshop.name }}】</span>
              <span class="ellipsis-text">{{ line.name }}</span>
            </template>
            <template v-slot="scope">
              <el-input-number
                v-if="modifying && !scope.row.boolAbnormalEnum"
                v-model="scope.row.schedulingMap[line.id].quantity"
                :step="1"
                :min="scope.row.schedulingMap[line.id].sourceQuantity || 0"
                :max="scope.row.quantity"
                :precision="0"
                size="mini"
                controls-position="right"
                style="width: 100%"
                @change="handleQuantityChange(scope.row, line, $event)"
              />
              <span v-else>{{ scope.row.schedulingMap[line.id].quantity }}</span>
            </template>
          </el-table-column>
        </template>
      </template>
      <el-table-column min-width="1px" />
      <el-table-column
        v-if="columns.visible('producedQuantity')"
        key="producedQuantity"
        prop="producedQuantity"
        sortable="custom"
        fixed="right"
        label="已生产"
        align="center"
        width="105px"
      />
      <el-table-column
        v-if="columns.visible('usedQuantity')"
        key="usedQuantity"
        prop="usedQuantity"
        sortable="custom"
        fixed="right"
        label="已使用"
        align="center"
        width="105px"
      />
      <el-table-column
        v-if="columns.visible('unassignQuantity')"
        key="unassignQuantity"
        prop="unassignQuantity"
        sortable="custom"
        fixed="right"
        label="未分配"
        align="center"
        width="105px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sourceUnassignQuantity }}</span>
          <span v-if="modifying && scope.row.unassignQuantity !== scope.row.sourceUnassignQuantity">
            ▶
            <span :style="{ color: scope.row.unassignQuantity < scope.row.sourceUnassignQuantity ? '#11b95c' : 'red' }">
              {{ scope.row.unassignQuantity }}
            </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('assignQuantity')"
        key="assignQuantity"
        prop="assignQuantity"
        sortable="custom"
        fixed="right"
        label="已分配"
        align="center"
        width="105px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sourceAssignQuantity }}</span>
          <span v-if="modifying && scope.row.assignQuantity !== scope.row.sourceAssignQuantity">
            ▶
            <span :style="{ color: scope.row.assignQuantity > scope.row.sourceAssignQuantity ? '#11b95c' : 'red' }">
              {{ scope.row.assignQuantity }}
            </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        fixed="right"
        label="数量"
        align="center"
        width="70px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- pdf预览 -->
    <drawing-preview-fullscreen-dialog
        v-model="showDrawing"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling/assemble'
import { provide, ref } from 'vue'

import { componentTypeEnum, processTypeEnum } from '@enum-ms/mes'
// import checkPermission from '@/utils/system/check-permission'
import { assembleSchedulingPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useSchedulingIndex from '@compos/mes/scheduling/use-scheduling-index'
import pagination from '@crud/Pagination'
import { productFormat } from '@/utils/columns-format/mes'
import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'
import mHeader from '@/views/mes/scheduling-manage/scheduling/components/scheduling-header'
import useDrawing from '@compos/use-drawing'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'ASSEMBLE' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productType = componentTypeEnum.ASSEMBLE.V
provide('productType', productType)
provide('processType', processTypeEnum.ONCE.V)

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '一次工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaId'],
    invisibleColumns: ['specification', 'material', 'length', 'netWeight', 'totalNetWeight', 'drawingNumber', 'remark'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { lines, modifying, handleRowClassName, handelCellClassName, handleQuantityChange, selectable } = useSchedulingIndex()
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  .cell {
    line-height: 30px;
  }
}
::v-deep(.abnormal-row) {
  background: #ffecec;
}
::v-deep(.el-input__inner) {
  font-size: 14px;
}
</style>

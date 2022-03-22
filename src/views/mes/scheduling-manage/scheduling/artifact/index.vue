<template>
  <div class="app-container">
    <mHeader v-model:modifying="modifying" v-model:lines="lines" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      returnSourceData
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      :row-class-name="handleRowClassName"
      :cell-class-name="handelCellClassName"
      row-key="id"
      style="width: 100%"
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
    <drawing-pdf
      v-model="showDrawing"
      :serial-number="drawingRow?.serialNumber"
      :productId="drawingRow?.productId"
      :productType="drawingRow?.productType"
    />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling/artifact'
import { provide, ref } from 'vue'

import { componentTypeEnum, processTypeEnum } from '@enum-ms/mes'
// import checkPermission from '@/utils/system/check-permission'
import { artifactSchedulingPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useSchedulingIndex from '@compos/mes/scheduling/use-scheduling-index'
import pagination from '@crud/Pagination'
import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'
import mHeader from '@/views/mes/scheduling-manage/scheduling/components/scheduling-header'
import useDrawing from '@compos/use-drawing'
import drawingPdf from '@comp-base/drawing-pdf.vue'

const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'ARTIFACT' })

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productType = componentTypeEnum.ARTIFACT.V
provide('productType', productType)
provide('processType', processTypeEnum.TWICE.V)

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '二次工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaId'],
    invisibleColumns: [
      'areaName',
      'length',
      'netWeight',
      'grossWeight',
      'totalNetWeight',
      'totalGrossWeight',
      'drawingNumber',
      'surfaceArea',
      'remark'
    ],
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

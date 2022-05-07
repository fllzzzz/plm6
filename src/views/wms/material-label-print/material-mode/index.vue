<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header v-bind="$attrs" />
    <!--表格渲染-->
    <common-table
      :key="`material_label_print_material_mode_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
        <template #default="{ row, $index }">
          <!-- 是否已打印 -->
          <table-cell-tag :show="row.printedNumber >= row.number" name="已打印" type="printed" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>

      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" spec-merge fixed="left" :show-index="false" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project />
      <el-table-column
        v-if="columns.visible('receipt')"
        key="receipt"
        :show-overflow-tooltip="true"
        prop="receipt"
        label="单据编号"
        align="left"
        min-width="120"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['INBOUND', 'OUTBOUND', 'TRANSFER', 'RETURN']" :receipt="row.receipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="生成日期"
        align="center"
        width="130"
        sortable="custom"
      />
      <el-table-column
        v-if="columns.visible('printNumber')"
        key="printNumber"
        :show-overflow-tooltip="true"
        prop="printNumber"
        label="批量打印数量"
        align="center"
        fixed="right"
        min-width="120"
      >
        <template #default="{ row: { sourceRow: row } }">
          <common-input-number v-model="row.printNumber" :min="1" :max="999" />
        </template>
      </el-table-column>
      <!--打印-->
      <el-table-column label="操作" width="70px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <material-print-button
            v-bind="$attrs"
            :material="row"
            :number="row.printNumber"
            :copies="crud ? crud.props.copies : 1"
            submit-print-record
            @printed-success="handleRefresh(row)"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-label-print/material-mode'

import { defineEmits, ref, inject } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { materialColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialPrintButton from '@/components-system/wms/material-print-button.vue'

const emit = defineEmits(['printed-success'])

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 权限
const permission = inject('permission')
// 展开行
const expandRowKeys = ref([])

// 表格ref
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...materialColumns, ['createTime', 'parse-time']])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '打印标签-物料模式',
    sort: ['id.desc'],
    invisibleColumns: ['serialNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true, extraHeight: 60 })

CRUD.HOOK.beforeRefresh = () => {
  emit('printed-success')
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
  data.content.forEach((row) => {
    if (row.basicClass === matClsEnum.STEEL_COIL.V) {
      row.number = 1
    } else {
      row.number = row.quantity
    }
    // 已打印数量
    row.printedNumber = row.printedNumber || 0
    // 需打印数量 = 数量 - 已打印数量
    row.printNumber = row.number - row.printedNumber
    // 需要打印数量至少为1
    row.printNumber = row.printNumber > 0 ? row.printNumber : 1
  })
}

function handleRefresh(row) {
  if (row.printedNumber < row.number) {
    crud.refresh()
  }
}
</script>

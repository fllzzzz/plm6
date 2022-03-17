<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`material_label_print_receipt_mode_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
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
          <!-- 是否已打印,此处数量，服务端需要注意退货，退货的标签不需要打印 -->
          <table-cell-tag :show="row.pendingPrintedMaterialNumber === 0" name="已打印" type="printed" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('receiptType')"
        key="receiptType"
        :show-overflow-tooltip="true"
        prop="receiptType"
        width="120"
        label="单据类型"
        align="center"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: receiptTypeEnum, v: row.receiptType, f: 'DOC' }"></span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        min-width="120"
        label="单据编号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['INBOUND', 'OUTBOUND', 'TRANSFER', 'RETURN']" :receipt="row" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        min-width="120"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: matClsEnum, v: row.basicClass, bit: true, split: ' | ' }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.projects, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('pendingPrintedMaterialNumber')"
        key="pendingPrintedMaterialNumber"
        :show-overflow-tooltip="true"
        prop="pendingPrintedMaterialNumber"
        label="未打印物料数量"
        align="center"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('reviewerName')"
        key="reviewerName"
        :show-overflow-tooltip="true"
        prop="reviewerName"
        label="审核人"
        align="center"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="创建日期"
        align="center"
        width="130"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time="row.createTime" />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="130" align="center" fixed="right">
        <template #default="{ row }">
          <ud-operation show-detail :show-edit="false" :show-del="false" :data="row" />
          <common-button icon="el-icon-printer" type="success" size="mini" @click="toPrintLabel(row.id)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail v-bind="$attrs" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-label-print/receipt-mode'

import { ref, inject, defineEmits } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MDetail from './module/detail.vue'
import UdOperation from '@crud/UD.operation.vue'
import MHeader from './module/header'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import usePrint from '../composables/use-print'
import useGetPrintInfo from '../composables/use-get-print-info'

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
const { CRUD, crud, columns } = useCRUD(
  {
    title: '打印标签-单据模式',
    sort: ['id.desc'],
    invisibleColumns: ['warehouse'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

crud.props.copies = 1
const { print } = usePrint({ emit })
const { getDetailMaterialList } = useGetPrintInfo()
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}

async function toPrintLabel(id) {
  await print(() => getDetailMaterialList(id), crud.props.copies)
  crud.refresh()
}
</script>

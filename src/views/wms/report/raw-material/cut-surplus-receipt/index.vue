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
      highlight-current-row
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
          <table-cell-tag
            v-if="isNotBlank(row.rejectStatus) && row.rejectStatus !== receiptRejectStatusEnum.NONE.V"
            :name="receiptRejectStatusEnum.VL[row.rejectStatus]"
            :color="receiptRejectStatusEnum.V[row.rejectStatus].COLOR"
          />
          {{ $index + 1 }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('cutTaskId')"
        key="cutTaskId"
        :show-overflow-tooltip="true"
        prop="cutTaskId"
        label="套料工单"
        width="175"
      >
        <template #default="{ row }">
          <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" :offset="10" />
          <span>{{ row.cutTaskId }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classifyName')"
        key="classifyName"
        :show-overflow-tooltip="true"
        prop="classifyName"
        width="120"
        label="名称"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="物料种类"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('sourceSpec')"
        key="sourceSpec"
        :show-overflow-tooltip="true"
        prop="sourceSpec"
        min-width="160"
        label="原钢板规格"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('spec')"
        key="spec"
        :show-overflow-tooltip="true"
        prop="spec"
        min-width="160"
        label="余料规格"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('cutEndTime')"
        key="cutEndTime"
        :show-overflow-tooltip="true"
        prop="cutEndTime"
        label="生成时间"
        align="center"
        width="140"
      />
      <!-- 仓库信息 -->
      <warehouse-info-columns showProject showMonomer showArea />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { getReceiptList as get, getReceiptDetail as detail } from '@/api/wms/report/raw-material/cut-surplus'
import { reportRawMaterialCutSurplusReceiptPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { receiptRejectStatusEnum } from '@enum-ms/wms'
import { isNotBlank } from '@/utils/data-type'
import { materialColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import TableCellTag from '@comp-common/table-cell-tag/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialColumns,
  ['cutEndTime', 'parse-time'],
  ['basicClass', ['parse-enum', rawMatClsEnum, { bit: true, split: ' | ' }]]
])

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '切割余料表',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get, detail }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

</script>

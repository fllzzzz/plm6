<template>
  <div class="report-material-transfer-details app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" show-project show-classification classify-name-alias="名称" :basic-class="basicClass" spec-merge sortable>
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('createTime')"
            key="createTime"
            :show-overflow-tooltip="true"
            prop="createTime"
            label="创建时间"
            align="center"
            width="140"
          >
            <template #default="{ row }">
              <factory-table-cell-tag :id="row.factory?.id" />
              <span>{{ row.createTime }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('supplementNo')"
            key="supplementNo"
            :show-overflow-tooltip="true"
            prop="supplementNo"
            label="调整单号"
            align="center"
            min-width="120"
          />
          <el-table-column
            v-if="columns.visible('receipt.serialNumber')"
            key="receipt.serialNumber"
            :show-overflow-tooltip="true"
            prop="receipt.serialNumber"
            label="退货单号"
            align="center"
            min-width="120"
          >
            <template #default="{ row }">
              <receipt-sn-clickable :receipt-types="['REJECTED']" :receipt="row.sourceRow?.receipt" />
            </template>
          </el-table-column>
        </template>
      </material-base-info-columns>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <el-table-column
        v-if="columns.visible('accountingUnit')"
        key="accountingUnit"
        :show-overflow-tooltip="true"
        prop="accountingUnit"
        label="核算单位"
        align="center"
        width="80"
      />
      <el-table-column
        v-if="columns.visible('mete')"
        key="mete"
        :show-overflow-tooltip="true"
        prop="mete"
        label="核算量"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('amount')"
        key="amount"
        :show-overflow-tooltip="true"
        prop="amount"
        :label="`调整金额(含税)`"
        align="right"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('amountExcludingVAT')"
        key="amountExcludingVAT"
        :show-overflow-tooltip="true"
        prop="amountExcludingVAT"
        :label="`调整金额(不含税)`"
        align="right"
        width="120"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { get } from '@/api/wms/report/raw-material/supplement'
import { getReceiptDetail as detail } from '@/api/wms/report/raw-material/reject'
import { reportRawMaterialSupplementDetailsPM as permission } from '@/page-permission/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialColumns } from '@/utils/columns-format/wms'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

// 表格列数据格式转换
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', decimalPrecision.value.wms]],
    ['amountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['inputVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['createTime', 'parse-time']
  ]
})
// const columnsDataFormat = ref([
//   ...materialHasAmountColumns,
//   ['createTime', 'parse-time']
// ])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '调整记录',
    sort: [],
    permission: { ...permission },
    invisibleColumns: [],
    optShow: { ...optShow },
    crudApi: { get, detail }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 基础类型
const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = data.content.map(v => {
    // 调整单号和物料种类字段冲突了
    v.supplementNo = v.serialNumber
    return v
  })
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
}
</script>


<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      highlight-current-row
      row-key="id"
      show-summary
      :summary-method="getSummaries"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" show-classification classify-name-alias="名称" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns />
      <!-- 价格信息 -->
      <amount-info-columns :columns="columns" :basic-class="basicClass" show-unit-price-e :showInput-v-a-t="false" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project show-monomer show-area />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { get } from '@/api/wms/report/raw-material/inventory'
import { reportRawMaterialInventoryPM as permission } from '@/page-permission/wms'

import { ref, computed } from 'vue'
import { materialColumns } from '@/utils/columns-format/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { tableSummary } from '@/utils/el-extra'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { DP } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
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
// const columnsDataFormat = ref([
//   ...materialHasAmountColumns
// ])
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', DP.YUAN]],
    ['amountExcludingVAT', ['to-thousand', DP.YUAN]],
    ['inputVAT', ['to-thousand', DP.YUAN]]
  ]
})

const { CRUD, crud, columns } = useCRUD(
  {
    title: '库存报表',
    sort: ['id.desc'],
    invisibleColumns: ['createTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'mete', ['amount', DP.YUAN], ['amountExcludingVAT', DP.YUAN]],
    toThousandFields: ['amount', 'amountExcludingVAT']
  })
}

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
}
</script>

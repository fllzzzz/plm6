<template>
  <!--表格渲染-->
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
      <material-base-info-columns />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns />
      <el-table-column
        show-overflow-tooltip
        key="unitPriceExcludingVAT"
        prop="unitPriceExcludingVAT"
        label="单价（不含税）"
        min-width="130"
        align="right"
      />
      <el-table-column
        show-overflow-tooltip
        key="amountExcludingVAT"
        prop="amountExcludingVAT"
        label="金额（不含税）"
        min-width="130"
        align="right"
      />
      <!-- 仓库信息 -->
      <warehouse-info-columns v-if="detailRow.type === orderDetailEnum.STOCK.V"/>
      <el-table-column
        v-else
        show-overflow-tooltip
        key="createTime"
        prop="createTime"
        :label="`${orderDetailEnum.V?.[detailRow.type].SL}日期`"
        width="150"
        align="center"
      />
    </common-table>
  <!--分页组件-->
  <pagination />
</template>

<script setup>
import { detail } from '@/api/wms/report/raw-material/psi'
import { ref, defineProps, watch, inject } from 'vue'

import { purchaseAcceptanceLogPM as permission } from '@/page-permission/supply-chain'
import { tableSummary } from '@/utils/el-extra'
import { STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { toThousand, getDP } from '@data-type/number'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  detailRow: {
    type: Object,
    define: () => {}
  }
})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const orderDetailEnum = inject('orderDetailEnum')
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ['amountExcludingVAT', ['to-thousand', decimalPrecision.wms]],
  ['createTime', 'parse-time']
])

const { CRUD, crud } = useCRUD(
  {
    title: '订单详情',
    sort: [],
    permission: { get: permission.detail },
    crudApi: { get: detail },
    invisibleColumns: [],
    queryOnPresenterCreated: false,
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

watch(
  () => props.detailRow,
  (data) => {
    crud.query = { ...props.detailRow }
    crud.refresh()
  },
  { immediate: true }
)

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = await setSpecInfoToList(data.content)
  data.content.forEach(v => {
    if (v.basicClass & STEEL_ENUM) {
      // 此页面钢材默认显示吨，保留3位
      v.accountingUnit = '千克'
      v.accountingPrecision = 3
    }
  })
  data.content = await numFmtByBasicClass(data.content, { toNum: true })
  data.content.forEach(v => {
    v.unitPriceExcludingVAT = toThousand(v.unitPriceExcludingVAT, getDP(v.unitPriceExcludingVAT))
  })
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    // 此页面钢材默认显示吨，保留3位，金额显示4位
    props: ['quantity', ['mete', 3], ['amountExcludingVAT', decimalPrecision.wms]],
    toThousandFields: ['amountExcludingVAT']
  })
}
</script>

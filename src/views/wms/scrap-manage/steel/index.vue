<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`scrap_steel_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" :show-project="crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V" />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="生成日期"
        align="center"
        width="140"
        sortable="custom"
      />
      <el-table-column
        v-if="columns.visible('convertNumber')"
        key="convertNumber"
        :show-overflow-tooltip="true"
        prop="convertNumber"
        label="来源"
        align="center"
        width="120"
        sortable="custom"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { getSteelList } from '@/api/wms/scrap-manage/list'
import { steelScrapPM as permission } from '@/page-permission/wms'

import { ref, computed } from 'vue'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { baseTimeColumns, materialColumns } from '@/utils/columns-format/wms'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { DP } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}
// 表格列数据格式转换
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
    ['inputVAT', ['to-thousand', DP.YUAN]],
    ...baseTimeColumns
  ]
})
// const columnsDataFormat = ref([...materialHasAmountColumns, ...baseTimeColumns])

// 展开行
const expandRowKeys = ref([])
// 表格ref
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '废料列表',
    sort: ['id.desc'],
    invisibleColumns: ['warehouse'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getSteelList }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}
</script>

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
      <material-base-info-columns :columns="columns" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :show-mete="false" />
      <el-table-column v-if="columns.visible('inboundMete')" prop="inboundMete" key="inboundMete" label="入库量" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>入库量 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="showDetail(row, orderDetailEnum.IN.V)">{{ row.inboundMete }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('outboundMete')" prop="outboundMete" key="outboundMete" label="出库量" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>出库量 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="showDetail(row, orderDetailEnum.OUT.V)">{{ row.outboundMete }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('inventoryMete')" prop="inventoryMete" key="inventoryMete" label="库存量" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>库存量 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="showDetail(row, orderDetailEnum.STOCK.V)">{{ row.inventoryMete }}</div>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 详情 -->
    <common-drawer
      v-model:visible="drawerVisible"
      :title="orderDetailEnum.VL?.[detailRow.type]"
      direction="rtl"
      size="80%"
      :before-close="
        () => {
          drawerVisible = false
        }
      "
    >
      <template #titleRight>
        <export-button v-permission="permission.download" :params="{...detailRow, times: crud.query.times}" :fn="download">下载</export-button>
      </template>
      <template #content>
        <detail :detail-row="detailRow" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { get, download } from '@/api/wms/report/raw-material/psi'
import { ref, reactive, provide } from 'vue'

import { reportRawMaterialPsiPM as permission } from '@/page-permission/wms'
import { tableSummary } from '@/utils/el-extra'
import checkPermission from '@/utils/system/check-permission'
import { constantize } from '@/utils/enum/base'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from './module/detail'
import ExportButton from '@comp-common/export-button/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const detailRow = ref({})
const drawerVisible = ref(false)
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ['amountExcludingVAT', 'to-thousand'],
  ['inboundTime', 'parse-time']
])

const orderDetailEnum = reactive({
  IN: { L: '入库明细', SL: '入库', K: 'IN', V: 1 },
  OUT: { L: '出库明细', SL: '出库', K: 'OUT', V: 2 },
  STOCK: { L: '库存明细', SL: '库存', K: 'STOCK', V: 3 }
})
constantize(orderDetailEnum)
provide('orderDetailEnum', orderDetailEnum)

const { crud, columns } = useCRUD(
  {
    title: '收发存记录',
    sort: [],
    permission: { ...permission },
    crudApi: { get },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    // 此页面钢材默认显示吨，保留3位，金额显示4位
    props: ['quantity', ['mete', 3], ['inboundMete', 3], ['outboundMete', 3], ['inventoryMete', 3]]
  })
}

// 打开详情
function showDetail(row, type) {
  if (!checkPermission(permission.detail)) return
  detailRow.value = { ...row?.sourceRow, type, times: crud.query.times }
  drawerVisible.value = true
}
</script>

<style lang="scss" scoped>
  .clickable {
    cursor: pointer;
  }
</style>

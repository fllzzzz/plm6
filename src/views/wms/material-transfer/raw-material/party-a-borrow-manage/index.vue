<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`party_a_borrow_${crud.query.basicClass}`"
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
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info :basic-class="row.basicClass" :row="row" show-graphics>
            <p>
              借用调拨备注：<span>{{ row.remark }}</span>
            </p>
            <p>
              归还调拨单号：
              <receipt-sn-clickable :receipt-types="['TRANSFER']" :receipt="row.returnTransfers" />
            </p>
          </expand-secondary-info>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" :show-party-a="false" fixed="left" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" />
      <!-- 单位及其数量 -->
      <el-table-column v-if="columns.visible('outboundUnit')" prop="outboundUnit" label="单位" align="center" width="70px" />
      <el-table-column v-if="columns.visible('quantity')" prop="quantity" label="已还/总数" align="right" width="110px">
        <template #default="{ row }">
          <span class="returned-number">{{ row.corReturnedQuantity }}</span>
          &nbsp;/&nbsp;
          {{ row.corQuantity }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        show-overflow-tooltip
        key="project"
        prop="project"
        label="原项目"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('borrowProject')"
        show-overflow-tooltip
        key="borrowProject"
        prop="borrowProject"
        label="借用项目"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('borrowTransferSN')"
        key="borrowTransferSN"
        :show-overflow-tooltip="true"
        prop="borrowTransferSN"
        label="借用调拨单号"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['TRANSFER']" :receipt="row.borrowTransfer" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('returnTransferSN')"
        key="returnTransferSN"
        :show-overflow-tooltip="true"
        prop="returnTransferSN"
        label="归还调拨单号"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['TRANSFER']" :receipt="row.returnTransfers" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferorName')"
        key="transferorName"
        :show-overflow-tooltip="true"
        prop="transferorName"
        label="借用调拨人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('transferTime')"
        key="transferTime"
        :show-overflow-tooltip="true"
        prop="transferTime"
        label="借用日期"
        align="center"
        width="100"
        sortable="custom"
      />
      <el-table-column
        v-if="columns.visible('returnTime')"
        key="returnTime"
        :show-overflow-tooltip="true"
        prop="returnTime"
        label="归还日期"
        align="center"
        width="100"
        sortable="custom"
      />
      <!-- 归还 -->
      <el-table-column label="操作" width="80px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <template v-if="checkPermission(permission.return) && row.returnStatus === borrowReturnStatusEnum.NOT_RETURNED.V">
            <common-button type="primary" size="mini" @click="toReturn(row)">归还</common-button>
          </template>
          <template v-else>
            <el-tag :type="borrowReturnStatusEnum.V[row.returnStatus].TAG">{{ borrowReturnStatusEnum.VL[row.returnStatus] }}</el-tag>
          </template>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <return-material v-model:visible="returnMaterialVisible" :detail="currentRow" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-transfer/raw-material/party-a-borrow-manage'
import { rawMaterialPartyABorrowPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { borrowReturnStatusEnum, measureTypeEnum } from '@/utils/enum/modules/wms'
import { materialColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import returnMaterial from './module/return-material.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 展开行
const expandRowKeys = ref([])
// 归还物料窗口显示
const returnMaterialVisible = ref(false)
// 当前行
const currentRow = ref()
// 表格ref
const tableRef = ref()
// 表格列格式化
const columnsDataFormat = ref([
  ...materialColumns,
  ['corReturnedQuantity', ['to-fixed', 'outboundUnitPrecision']],
  ['corQuantity', ['to-fixed', 'outboundUnitPrecision']],
  ['remark', 'empty-text'],
  ['project', ['parse-project', { onlyShortName: true }]],
  ['borrowProject', ['parse-project', { onlyShortName: true }]],
  ['transferTime', ['parse-time', '{y}-{m}-{d}']],
  ['returnTime', ['parse-time', '{y}-{m}-{d}']]
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '甲供物料借出管理',
    sort: ['id.desc'],
    invisibleColumns: ['transferorName', 'transferTime', 'returnTransferSN', 'returnTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
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
  data.content.forEach((v) => {
    v.pendingQuantity = v.quantity - v.returnedQuantity
    v.pendingMete = v.mete - v.returnedMete
    if (v.outboundUnitType === measureTypeEnum.MEASURE.V) {
      v.corQuantity = v.quantity // 数量
      v.corReturnedQuantity = v.returnedQuantity // 已还数量
      v.corPendingQuantity = v.pendingQuantity // 待还数量
      v.corUnderReviewQuantity = v.underReviewQuantity // 审核中的数量
    } else {
      // 核算量
      v.corQuantity = v.mete
      v.corReturnedQuantity = v.returnedMete
      v.corPendingQuantity = v.pendingMete
      v.corUnderReviewQuantity = v.underReviewMete
    }
  })
}

// 归还
function toReturn(row) {
  returnMaterialVisible.value = true
  currentRow.value = row
}
</script>

<style lang="scss" scoped>
.returned-number {
  color: green;
}
</style>

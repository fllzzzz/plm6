<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader @showDetail="showDetailSummary" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="rowId"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="合同编号"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.shortName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('factoryName') && crud.query.factoryId"
        key="factoryName"
        prop="factoryName"
        :show-overflow-tooltip="true"
        label="工厂"
      >
        <template v-slot="scope">
          <span>{{ scope.row.factoryName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        :show-overflow-tooltip="true"
        align="center"
        label="类型"
        width="100"
      >
        <template v-slot="scope">
          <span>{{ reportComponentTypeEnum.VL[scope.row.productType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unit')"
        key="unit"
        prop="unit"
        :show-overflow-tooltip="true"
        label="单位"
        align="center"
        width="100"
      >
        <template v-slot="scope">
          <span>{{ scope.row.unit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('beginMete')"
        key="beginMete"
        prop="beginMete"
        :show-overflow-tooltip="true"
        label="期初库存"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.BEGIN.V)">
            <span>{{ scope.row.beginMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundMete')"
        key="inboundMete"
        prop="inboundMete"
        :show-overflow-tooltip="true"
        label="入库量"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.INBOUND.V)">
            <span>{{ scope.row.inboundMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundMete')"
        key="outboundMete"
        prop="outboundMete"
        :show-overflow-tooltip="true"
        label="出库量"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.OUTBOUND.V)">
            <span>{{ scope.row.outboundMete }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('endMete')"
        key="endMete"
        prop="endMete"
        :show-overflow-tooltip="true"
        label="期末库存"
        align="center"
      >
        <template v-slot="scope">
          <div style="cursor: pointer" @click="showDetail(scope.row, reportTypeEnum.END.V)">
            <span> {{ scope.row.endMete }}</span>
          </div>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :report-type="reportType" :item-info="itemInfo" :is-summary="isSummary" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/manufactures-manage/report'
import { ref, provide } from 'vue'

import { reportWarehouseStatePM as permission } from '@/page-permission/mes'
import { reportComponentTypeEnum } from '@enum-ms/mes'
import { constantize } from '@enum/base'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mDetail from './module/detail/index'

const reportTypeEnum = {
  BEGIN: { L: '期初库存', K: 'BEGIN', V: 1 },
  INBOUND: { L: '入库', K: 'INBOUND', V: 2 },
  OUTBOUND: { L: '出库', K: 'OUTBOUND', V: 3 },
  END: { L: '期末库存', K: 'END', V: 4 }
}
constantize(reportTypeEnum)
provide('reportTypeEnum', reportTypeEnum)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '入发存报表',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    dataPath: ''
  },
  tableRef
)
provide('query', crud.query)

const { maxHeight } = useMaxHeight({ paginate: false })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data = res.data.map((v, i) => {
    v.rowId = i + '' + Math.random()
    return v
  })
}

const detailVisible = ref(false)
const reportType = ref()
const itemInfo = ref({})
const isSummary = ref(false)

function showDetail(row, type) {
  if (!checkPermission(permission.detail)) return
  detailVisible.value = true
  reportType.value = type
  itemInfo.value = row
  isSummary.value = false
}
function showDetailSummary(type) {
  if (!checkPermission(permission.detail)) return
  detailVisible.value = true
  reportType.value = type
  itemInfo.value = {}
  isSummary.value = true
}
</script>

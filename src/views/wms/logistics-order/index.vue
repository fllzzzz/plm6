<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="!loaded || crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column type="expand">
        <template #header>
          <el-icon class="pointer" @click="handleExpandAll"><el-arrow-down v-if="expandAll" /><el-arrow-right v-else /></el-icon>
        </template>
        <template #default="scope">
          <div class="table-expand-container">
            <p>
              关联项目：<span v-empty-text>{{ scope.row.projectStr }}</span>
            </p>
            <p>
              备注：<span v-empty-text>{{ scope.row.remark }}</span>
            </p>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="日期"
        align="center"
        width="100"
      >
        <template #default="scope">
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateNumber')"
        key="plateNumber"
        :show-overflow-tooltip="true"
        prop="plateNumber"
        label="车牌号"
        align="left"
        min-width="100"
      />
      <el-table-column v-if="columns.visible('projects')" key="projects" prop="projects" label="关联项目" min-width="170">
        <template #default="scope">
          <span class="ellipsis-text">
            <span v-for="item in scope.row.projects" :key="item.id"> 【{{ item.shortName }}】 </span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('loadingWeight')"
        key="loadingWeight"
        :show-overflow-tooltip="true"
        prop="loadingWeight"
        label="装载重量(kg)"
        min-width="120"
        align="right"
      >
        <template #default="scope">
          <span v-empty-text v-to-fixed>{{ scope.row.loadingWeight }}</span>
        </template>
      </el-table-column>

      <el-table-column
        v-if="columns.visible('invoiceType')"
        key="invoiceType"
        :show-overflow-tooltip="true"
        prop="invoiceType"
        label="票据类型"
        align="center"
        min-width="130"
      >
        <template #default="scope">
          <span v-parse-enum="{ e: invoiceTypeEnum, v: scope.row.invoiceType }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taxRate')"
        key="taxRate"
        :show-overflow-tooltip="true"
        prop="taxRate"
        label="税率"
        align="right"
        width="70"
      >
        <template #default="scope">
          <span v-empty-text>{{ scope.row.taxRate ? `${scope.row.taxRate}%` : undefined }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('freight')"
        key="freight"
        :show-overflow-tooltip="true"
        prop="freight"
        label="运费(含税)"
        align="right"
        min-width="90"
      >
        <template #default="scope">
          <span v-to-fixed>{{ scope.row.freight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('priceExcludingVAT')"
        key="priceExcludingVAT"
        :show-overflow-tooltip="true"
        prop="priceExcludingVAT"
        label="不含税金额"
        min-width="90"
        align="right"
      >
        <template #default="scope">
          <span v-to-fixed>{{ scope.row.priceExcludingVAT }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inputVAT')"
        key="inputVAT"
        :show-overflow-tooltip="true"
        prop="inputVAT"
        label="进项税额"
        align="right"
        min-width="80"
      >
        <template #default="scope">
          <span v-to-fixed>{{ scope.row.inputVAT }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="物流单号"
        align="center"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('purchaseSN')"
        key="purchaseSN"
        :show-overflow-tooltip="true"
        prop="purchaseSN"
        label="采购单号"
        align="center"
        min-width="170"
      >
        <template #default="scope">
          <span class="text-clickable" @click="openInboundDetailView">{{ scope.row.inboundSN }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundSN')"
        key="inboundSN"
        :show-overflow-tooltip="true"
        prop="inboundSN"
        label="入库单号"
        align="center"
        min-width="170"
      >
        <template #default="scope">
          <span class="text-clickable" @click="openInboundDetailView">{{ scope.row.inboundSN }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="物流公司"
        min-width="200"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/logistics-order'
import { ref, computed } from 'vue'
import EO from '@enum'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { baseMaterialTypeEnum } from '@enum-ms/wms'
import { matClsEnum } from '@enum-ms/classification'
import { projectNameFormatter } from '@/utils/project'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useSuppliers from '@compos/store/use-suppliers'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const permission = {
  get: ['wms_logisticsOrder:get'],
  edit: ['wms_logisticsOrder:edit'],
  del: ['wms_logisticsOrder:del'],
  add: ['wms_logisticsOrder:add']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '物料物流订单',
    sort: ['id.desc'],
    invisibleColumns: ['projects', 'purchaseSN'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const expandRowKeys = ref([])
const expandAll = computed(() => expandRowKeys.value.length === crud.data.length)
const { loaded, supplierKV } = useSuppliers()
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.materialTypeText = baseMaterialTypeEnum.VL[v.purchaseType] + ' - ' + basicClassArr.join(' | ')
    v.projectStr = v.projects ? v.projects.map((v) => projectNameFormatter(v, null, false)).join('　、　') : ''
    v.supplier = computed(() => supplierKV.value[v.supplierId])
    return v
  })
}

// TODO:打开入库详情窗口
function openInboundDetailView() {}

// 展开所有行
function handleExpandAll() {
  if (!expandAll.value) {
    expandRowKeys.value = crud.data.map((v) => v.id)
  } else {
    expandRowKeys.value = []
  }
}
</script>

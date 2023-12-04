<template>
  <div>
    <!--工具栏-->
    <mHeader ref="headerRef" class="auxiliary-material-container" />
    <!--表格渲染-->
    <common-table ref="tableRef" v-loading="crud.loading" :data="crud.data" :data-format="dataFormat" :max-height="maxHeight">
      <el-table-column fixed="left" label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('project') && !crud.query.projectId"
        key="project"
        prop="project"
        show-overflow-tooltip
        label="项目"
        align="center"
        min-width="150"
      />
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        show-overflow-tooltip
        label="单体"
        align="center"
        min-width="130"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        key="area.name"
        prop="area.name"
        show-overflow-tooltip
        label="区域"
        align="center"
        min-width="130"
      />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        show-overflow-tooltip
        label="名称"
        align="center"
        min-width="140"
      >
        <template #default="{row}">
          <table-cell-tag v-if="row.boolReturn" name="退量" color="#f56c6c"/>
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        show-overflow-tooltip
        label="规格"
        align="center"
        min-width="140"
      />
      <el-table-column
        v-if="columns.visible('measureUnit')"
        key="measureUnit"
        prop="measureUnit"
        show-overflow-tooltip
        label="单位"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        label="数量"
        align="center"
        min-width="110"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('unitPrice')"
        key="unitPrice"
        prop="unitPrice"
        align="right"
        min-width="120"
        label="综合单价"
      />
      <el-table-column v-if="columns.visible('totalPrice')" key="totalPrice" prop="totalPrice" align="right" min-width="120" label="金额" />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        align="center"
        label="发运日期"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('auditUserName')"
        key="auditUserName"
        prop="auditUserName"
        show-overflow-tooltip
        label="发运人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('actualUserName')"
        key="actualUserName"
        prop="actualUserName"
        show-overflow-tooltip
        label="过磅人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('supplierName')"
        key="supplierName"
        prop="supplierName"
        show-overflow-tooltip
        label="物流公司"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('cargoSerialNumber')"
        key="cargoSerialNumber"
        prop="cargoSerialNumber"
        show-overflow-tooltip
        label="车次"
        align="center"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        prop="licensePlate"
        show-overflow-tooltip
        label="车牌"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('workshopNames')"
        key="workshopNames"
        prop="workshopNames"
        show-overflow-tooltip
        label="生产部门"
        align="center"
        min-width="120"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { auxiliaryMaterialList as get } from '@/api/contract/sales-manage/shipment-tracking'
import { ref, defineEmits, computed } from 'vue'

import { shipmentTrackingPM as permission } from '@/page-permission/contract'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['resetQuery'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const dataFormat = computed(() => {
  return [
    ['project', 'parse-project'],
    ['createTime', ['parse-time', '{y}-{m}-{d}']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.contract]],
    ['totalPrice', ['to-thousand', decimalPrecision.value.contract]]
  ]
})

const { crud, columns, CRUD } = useCRUD(
  {
    title: '配套制品',
    sort: [],
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    invisibleColumns: ['actualUserName', 'supplierName', 'cargoSerialNumber', 'workshopNames']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.common-container', '.auxiliary-material-container'],
  minHeight: 300,
  paginate: true
})

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  console.log(data)
  data.content.forEach((row) => {
    if (row.boolReturn && row.totalPrice > 0) {
      row.totalPrice = row.totalPrice * -1
    }
  })
}

// 重置前
CRUD.HOOK.beforeResetQuery = () => {
  emit('resetQuery')
}
</script>

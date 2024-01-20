<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader @zoomChangeSize="getZoomIn" />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="flag ? 360 : maxHeight"
      show-summary
      :summary-method="getSummaries"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('project')"
        header-align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="120"
      >
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        align="center"
        key="productionLine.name"
        prop="productionLine.name"
        :show-overflow-tooltip="true"
        label="生产线"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLine ? scope.row.productionLine?.name : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        align="center"
        :show-overflow-tooltip="true"
        label="单体"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        align="center"
        key="area.name"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
      />
      <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="名称">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        align="center"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        align="center"
        key="specification"
        prop="specification"
        :show-overflow-tooltip="true"
        label="规格"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('length')"
        header-align="center"
        key="length"
        prop="length"
        :show-overflow-tooltip="true"
        label="长度（mm）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('material')"
        align="center"
        key="material"
        prop="material"
        :show-overflow-tooltip="true"
        label="材质"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        align="center"
        key="quantity"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="数量"
      >
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
         v-if="columns.visible('surfaceArea')"
        align="center"
        key="surfaceArea"
        prop="surfaceArea"
        :show-overflow-tooltip="true"
        label="单面积"
      >
        <template #default="{row}">
          {{ row.surfaceArea }}
        </template>
      </el-table-column> -->
      <el-table-column
         v-if="columns.visible('totalSurfaceArea')"
        align="center"
        key="totalSurfaceArea"
        prop="totalSurfaceArea"
        :show-overflow-tooltip="true"
        label="总面积"
      >
        <template #default="{row}">
          {{ row.totalSurfaceArea.toFixed(2) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('netWeight')"
        align="center"
        key="netWeight"
        prop="netWeight"
        :show-overflow-tooltip="true"
        label="单重（kg）"
      >
        <template v-slot="scope">
          <span>{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? scope.row.netWeight?.toFixed(2) : scope.row.grossWeight?.toFixed(2)
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        align="center"
        key="totalNetWeight"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        label="总重（kg）"
      >
        <template v-slot="scope">
          <span>{{
            crud.query.weightStatus === weightTypeEnum.NET.V ? scope.row.totalNetWeight?.toFixed(2) : scope.row.totalGrossWeight?.toFixed(2)
          }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('date')" align="center" key="date" prop="date" :show-overflow-tooltip="true" label="入库日期">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.date, '{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/factory-report/workshop-report.js'
import { ref } from 'vue'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import { weightTypeEnum } from '@enum-ms/common'
import { projectNameFormatter } from '@/utils/project'
import { mesFactoryReportPM as permission } from '@/page-permission/mes'
import mHeader from './module/header'

const tableRef = ref()
const flag = ref()
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '车间报表',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    invisibleColumns: ['productionLine.name', 'date'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ minHeight: '15', extraBox: ['.head-container'], paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['length', 'quantity', ['netWeight', 2], ['grossWeight', 2], ['totalNetWeight', 2], ['totalGrossWeight', 2]]
  })
}

function getZoomIn(val) {
  flag.value = val
}
</script>

<style>
</style>

<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
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
        min-width="140"
      >
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        align="center"
        key="monomer.name"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomer?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('area.name')"
        align="center"
        key="area.name"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
      >
        <template v-slot="scope">
          <span>{{ scope.row.area?.name }}</span>
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
        min-width="140"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('netWeight')"
        align="center"
        key="netWeight"
        prop="netWeight"
        :show-overflow-tooltip="true"
        label="重量（kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        align="center"
        key="productionLine.name"
        prop="productionLine.name"
        :show-overflow-tooltip="true"
        label="产线"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLine?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeTime')"
        align="center"
        key="completeTime"
        prop="completeTime"
        :show-overflow-tooltip="true"
        label="生产日期"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.completeTime, '{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auxiliaryTypeName')"
        align="center"
        key="auxiliaryTypeName"
        prop="auxiliaryTypeName"
        :show-overflow-tooltip="true"
        label="类型"
      >
        <template v-slot="scope">
          <span>{{ auxiliaryMaterialTypeEnum.VL[scope.row.auxiliaryTypeName] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auxiliarySpecification')"
        align="center"
        key="auxiliarySpecification"
        prop="auxiliarySpecification"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="80"
      >
        <template v-slot="scope">
          <span>{{ scope.row.auxiliarySpecification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auxiliaryQuantity')"
        align="center"
        key="auxiliaryQuantity"
        prop="auxiliaryQuantity"
        :show-overflow-tooltip="true"
        label="数量"
      >
        <template v-slot="scope">
          <span>{{ scope.row.auxiliaryQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('price')"
        align="center"
        key="price"
        prop="price"
        :show-overflow-tooltip="true"
        label="单价"
      >
        <template v-slot="scope">
          <span>{{ scope.row.price }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('wages')"
        align="center"
        key="wages"
        prop="wages"
        :show-overflow-tooltip="true"
        label="总价"
      >
        <template v-slot="scope">
          <span>{{ scope.row.wages }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/production-line-wage-statistics/stud-sleeve-statistics'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import { auxiliaryMaterialTypeEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import mHeader from './module/header'

const tableRef = ref()
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '栓钉套筒统计',
    sort: [],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    // permission: { ...permission },
    hasPagination: true
  },
  tableRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['auxiliaryQuantity', 'wages']
  })
}
</script>

<style>
</style>

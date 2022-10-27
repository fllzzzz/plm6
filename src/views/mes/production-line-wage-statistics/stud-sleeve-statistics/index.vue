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
        v-if="columns.visible('projectName')"
        header-align="center"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomerName')"
        header-align="center"
        key="monomerName"
        prop="monomerName"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('areaName')"
        header-align="center"
        key="areaName"
        prop="areaName"
        :show-overflow-tooltip="true"
        label="区域"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        header-align="center"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        header-align="center"
        key="specification"
        prop="specification"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('weight')"
        header-align="center"
        key="weight"
        prop="weight"
        :show-overflow-tooltip="true"
        label="重量（kg）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.weight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLine')"
        header-align="center"
        key="productionLine"
        prop="productionLine"
        :show-overflow-tooltip="true"
        label="产线"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLine }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionDate')"
        header-align="center"
        key="productionDate"
        prop="productionDate"
        :show-overflow-tooltip="true"
        label="生产日期"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionDate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('stud')"
        header-align="center"
        key="stud"
        prop="stud"
        :show-overflow-tooltip="true"
        label="栓钉"
        min-width="60"
      >
        <el-table-column
          v-if="columns.visible('specification')"
          header-align="center"
          key="specification"
          prop="specification"
          :show-overflow-tooltip="true"
          label="规格"
          min-width="60"
        >
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('studQuantity')"
          header-align="center"
          key="studQuantity"
          prop="studQuantity"
          :show-overflow-tooltip="true"
          label="数量"
          min-width="60"
        >
          <template v-slot="scope">
            <span>{{ scope.row.studQuantity }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('sleeve')"
        header-align="center"
        key="sleeve"
        prop="sleeve"
        :show-overflow-tooltip="true"
        label="套筒"
        min-width="60"
      >
        <el-table-column
          v-if="columns.visible('specification')"
          header-align="center"
          key="specification"
          prop="specification"
          :show-overflow-tooltip="true"
          label="规格"
          min-width="60"
        >
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('sleeveQuantity')"
          header-align="center"
          key="sleeveQuantity"
          prop="sleeveQuantity"
          :show-overflow-tooltip="true"
          label="数量"
          min-width="60"
        >
          <template v-slot="scope">
            <span>{{ scope.row.sleeveQuantity }}</span>
          </template>
        </el-table-column>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
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
    //   crudApi: { ...crudApi },
    // permission: { ...permission },
    hasPagination: true
  },
  tableRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['studQuantity', 'sleeveQuantity']
  })
}
</script>

<style>
</style>

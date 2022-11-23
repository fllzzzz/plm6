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
      max-height="330px"
      show-summary
      :summary-method="getSummaries"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('project')"
        header-align="center"
        key="project.shortName"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('name')"
        header-align="center"
        key="name"
        prop="name"
        align="center"
        :show-overflow-tooltip="true"
        label="名称"
      >
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        header-align="center"
        key="serialNumber"
        prop="serialNumber"
        align="center"
        :show-overflow-tooltip="true"
        label="构件编号"
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
        align="center"
        :show-overflow-tooltip="true"
        label="规格"
      >
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        header-align="center"
        key="length"
        prop="length"
        align="center"
        :show-overflow-tooltip="true"
        label="长度（mm）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        header-align="center"
        key="material"
        prop="material"
        align="center"
        :show-overflow-tooltip="true"
        label="材质"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        header-align="center"
        key="quantity"
        prop="quantity"
        align="center"
        :show-overflow-tooltip="true"
        label="生产数"
      >
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('mete')"
        header-align="center"
        key="mete"
        prop="mete"
        align="center"
        :show-overflow-tooltip="true"
        label="总重（kg）"
      >
        <template v-slot="scope">
          <span>{{ (scope.row.mete).toFixed(2) }}</span>
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
import { tableSummary } from '@/utils/el-extra'
import { projectNameFormatter } from '@/utils/project'
import mHeader from './module/header'

const tableRef = ref()
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD({
  title: '车间报表',
  sort: [],
  optShow: { ...optShow },
  crudApi: { ...crudApi },
  hasPagination: true
},
tableRef
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['length', 'quantity', ['mete', 2]]
  })
}
</script>

<style>
</style>

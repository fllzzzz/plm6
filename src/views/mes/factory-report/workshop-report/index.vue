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
          <span>{{ scope.row.projectNumber }}-{{ scope.row.projectName }}</span>
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
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('artifactNumber')"
        header-align="center"
        key="artifactNumber"
        prop="artifactNumber"
        align="center"
        :show-overflow-tooltip="true"
        label="构件编号"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.artifactNumber }}</span>
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
        min-width="60"
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
        min-width="60"
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
        min-width="60"
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
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalWeight')"
        header-align="center"
        key="totalWeight"
        prop="totalWeight"
        align="center"
        :show-overflow-tooltip="true"
        label="总重（kg）"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalWeight }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
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

const { crud, columns } = useCRUD({
  title: '车间报表',
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
    props: ['quantity', 'totalWeight']
  })
}
</script>

<style>
</style>

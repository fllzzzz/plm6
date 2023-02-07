<template>
  <div class="app-container">
    <div class="head-container">
      <crudOperation>
        <template #viewLeft>
          <export-button class="filter-item"> 辅材消耗分析清单 </export-button>
        </template>
      </crudOperation>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column type="index" label="序号" prop="index" align="center" width="60px" />
      <el-table-column v-if="columns.visible('projectName')" label="项目" prop="projectName" align="center" />
      <el-table-column v-if="columns.visible('classify')" label="分类" prop="classify" align="center" />
      <el-table-column v-if="columns.visible('unit')" label="单位" prop="unit" align="center" />
      <el-table-column v-if="columns.visible('month')" label="月份" prop="month" align="center">
        <el-table-column :label="item" align="center" v-for="item in monthArr" :key="item">
        </el-table-column>
      </el-table-column>
      <el-table-column v-if="columns.visible('allYearAverage')" label="全年平均" prop="allYearAverage" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { tableSummary } from '@/utils/el-extra'
import crudOperation from '@crud/CRUD.operation'
import ExportButton from '@comp-common/export-button/index.vue'

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i)
}
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '管理费',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

<template>
  <div class="app-container">
    <mHeader class="head-container" />
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      show-summary
      :summary-method="getSummaries"
      :data-format="dataFormat"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project.shortName"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        align="center"
      />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" align="center" :show-overflow-tooltip="true" label="名称" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        align="center"
        :show-overflow-tooltip="true"
        label="编号"
      />
      <el-table-column key="plate" prop="plate" v-if="columns.visible('plate')" show-overflow-tooltip label="板型" align="center" />
      <el-table-column key="brand" prop="brand" v-if="columns.visible('brand')" show-overflow-tooltip label="品牌" align="center" />
      <el-table-column key="color" prop="color" v-if="columns.visible('color')" show-overflow-tooltip label="颜色" align="center" />
      <el-table-column key="length" prop="length" v-if="columns.visible('length')" show-overflow-tooltip label="单长(mm)" align="right" />
      <el-table-column
        v-if="columns.visible('producedQuantity')"
        key="producedQuantity"
        prop="producedQuantity"
        align="center"
        :show-overflow-tooltip="true"
        label="数量"
      />
      <el-table-column
        key="totalLength"
        prop="totalLength"
        v-if="columns.visible('totalLength')"
        show-overflow-tooltip
        label="总长度(m)"
        align="right"
      />
      <el-table-column
        key="completeTime"
        prop="completeTime"
        v-if="columns.visible('completeTime')"
        show-overflow-tooltip
        label="生产日期"
        align="center"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-report/production-statistics'
import { ref } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { enclosureProductionStatisticsPM as permission } from '@/page-permission/enclosure'

import mHeader from './module/header'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dataFormat = ref([
  ['project', 'parse-project'],
  ['totalLength', ['to-fixed', 2]],
  ['completeTime', ['parse-time', '{y}-{m}-{d}']]
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '车间报表',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  minHeight: 200,
  paginate: true
})

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['totalLength', 2]]
  })
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.totalLength = (row.totalLength || 0) / 1000
  })
}
</script>

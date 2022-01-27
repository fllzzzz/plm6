<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="monomer.name" prop="monomer.name" :show-overflow-tooltip="true" align="left" label="单体" min-width="140" />
      <el-table-column key="area.name" prop="area.name" :show-overflow-tooltip="true" align="left" label="区域" min-width="120" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" align="left" label="名称" min-width="110" />
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" align="left" label="编号" min-width="110" />
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" align="left" label="规格" min-width="150" />
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" align="center" label="材质" min-width="110" />
      <el-table-column key="measure" prop="measure" label="计量单位" align="center" width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.measure }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" label="数量" align="center" width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="nuclear" prop="nuclear" label="核算单位" align="center" width="70" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.nuclear }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalMete')" key="totalMete" prop="totalMete" label="总量" align="center" min-width="70px">
        <template #default="{ row }">
          <span v-empty-text>{{ row.totalMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unitPrice')" prop="unitPrice" align="right" min-width="90" label="单价">
        <template #default="{ row }">
          <span v-thousand="row.unitPrice" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" align="right" min-width="100" label="总价">
        <template #default="{ row }">
          <span v-thousand="row.totalPrice" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" label="发运日期" align="center" width="100">
        <template #default="{ row }">
          <span v-parse-time="{ val: row.auditTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/cost/business-manage/business-tracking'
import { ref } from 'vue'

import { businessTrackingPM as permission } from '@/page-permission/cost'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '商务跟踪',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

function getSummaries(param) {
  return tableSummary(param, { props: ['totalPrice'], toThousandFields: ['totalPrice'] })
}

</script>

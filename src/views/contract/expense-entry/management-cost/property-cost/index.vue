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
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column v-if="columns.visible('month')" prop="month" label="月份" align="center" width="140px">
        <template #default="{ row }">
          <span>{{ row.month }}月</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('feeAmount')"
        align="center"
        key="feeAmount"
        prop="feeAmount"
        :show-overflow-tooltip="true"
        label="费用"
      >
        <template #default="{ row }">
          <span>{{ toThousand(row.feeAmount, 0) }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作" width="300px">
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form :query="crud.query" :permission="permission" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/contract/expense-entry/property-cost'

import { toThousand } from '@data-type/number'
import { DP } from '@/settings/config'
import { tableSummary } from '@/utils/el-extra'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { propertyCostPM as permission } from '@/page-permission/contract'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const tableRef = ref()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '物业费',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['feeAmount', DP.YUAN]],
    toThousandFields: ['feeAmount']
  })
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    return v
  })
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>

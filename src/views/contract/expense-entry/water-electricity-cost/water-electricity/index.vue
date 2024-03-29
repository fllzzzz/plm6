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
      <el-table-column v-if="columns.visible('month')" prop="month" label="月份" align="center" width="100">
        <template #default="{ row }">
          <span>{{ row.month }}月</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('usedMete')"
        align="center"
        key="usedMete"
        prop="usedMete"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '用电度数（kw/h）' : '用水量（吨）'"
      >
        <template #default="{ row }">
          <span>{{ row.usedMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalAmount')"
        align="center"
        key="totalAmount"
        prop="totalAmount"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '电费总额（元）' : '水费总额（元）'"
      >
        <template #default="{ row }">
          <span>{{ row.totalAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('averageValue')"
        align="center"
        key="averageValue"
        prop="averageValue"
        :show-overflow-tooltip="true"
        :label="crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '平均电费（元/kw/h）' : '平均水费（元/吨）'"
      >
        <template #default="{ row }">
          <span>{{ row.averageValue }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作" width="200px">
        <template #default="{ row }">
          <udOperation :data="row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form :query="crud.query" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/contract/expense-entry/water-electricity-cost'

import { waterElectricityCostPM as permission } from '@/page-permission/contract'
import { costTypeEnum } from '@enum-ms/contract'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const tableRef = ref()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '水电费',
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
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 3) {
      sums[index] = 0
      const usedMeteList = data.map((v) => v.usedMete)
      const totalAmountList = data.map((v) => v.totalAmount)
      const usedMeteSum = usedMeteList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const totalAmountSum = totalAmountList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = usedMeteSum ? (totalAmountSum / usedMeteSum).toFixed(decimalPrecision.value.contract) : 0
      return
    }
    if (column.property === 'usedMete' || column.property === 'totalAmount') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      if (column.property === 'totalAmount') {
        sums[index] = valuesSum.toFixed(decimalPrecision.value.contract)
      } else {
        sums[index] = valuesSum.toFixed(2)
      }
    }
  })
  return sums
}
CRUD.HOOK.beforeToQuery = () => {}
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.averageValue = v.totalAmount && v.usedMete ? (v.totalAmount / v.usedMete).toFixed(decimalPrecision.value.contract) : 0
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

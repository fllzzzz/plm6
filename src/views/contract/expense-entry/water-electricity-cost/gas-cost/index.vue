<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader @change="getAccountingUnit" :gas-type-list="gasTypeList" />
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
      <el-table-column v-if="columns.visible('month')" prop="month" label="月份" align="center" width="100" />
      <el-table-column
        v-if="columns.visible('classifyName')"
        align="center"
        key="classifyName"
        prop="classifyName"
        :show-overflow-tooltip="true"
        label="气体种类"
      />
      <el-table-column
        v-if="columns.visible('accountingUnit')"
        align="center"
        key="accountingUnit"
        prop="accountingUnit"
        :show-overflow-tooltip="true"
        label="单位"
      />
      <el-table-column
        v-if="columns.visible('usedMete')"
        align="center"
        key="usedMete"
        prop="usedMete"
        :show-overflow-tooltip="true"
        label="使用量"
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
        label="总额"
      >
        <template #default="{ row }">
          <span>{{ row.totalAmount?.toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('avgUnitPrice')"
        align="center"
        key="avgUnitPrice"
        prop="avgUnitPrice"
        :show-overflow-tooltip="true"
        label="平均单价"
      >
        <template #default="{ row }">
          <span>{{ row.avgUnitPrice?.toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作" width="200px">
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form :query="crud.query" :accounting-unit="accountingUnit" :gas-type="gasType" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi, { getGasList } from '@/api/contract/expense-entry/gas-cost'

import { gasCostPM as permission } from '@/page-permission/contract'
// import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const tableRef = ref()
const gasTypeList = ref([])
const accountingUnit = ref()
const gasType = ref()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '气体统计',
    sort: [],
    optShow: { ...optShow },
    requiredQuery: ['classifyId'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

gasListGet()

async function gasListGet() {
  try {
    const { content } = await getGasList()
    gasTypeList.value = content || []
    crud.query.classifyId = gasTypeList.value[0]?.id
    crud.query.unit = gasTypeList.value[0]?.accountingUnit
    crud.toQuery()
  } catch (e) {
    console.log('获取气体类型失败', e)
  }
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
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
      sums[index] = valuesSum.toFixed(2)
    }
  })
  return sums
}

CRUD.HOOK.beforeToQuery = (crud) => {}
CRUD.HOOK.handleRefresh = async (crud, res) => {
  // res.data.content = await numFmtByBasicClass(
  //   res.data.content,
  //   {
  //     toSmallest: false,
  //     toNum: true
  //   },
  //   {
  //     mete: ['usedMete'],
  //     amount: ['avgUnitPrice']
  //   }
  // )
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function getAccountingUnit(val, gasVal) {
  accountingUnit.value = val
  gasType.value = gasVal
}
</script>
<style lang="scss" scoped>
</style>

<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader ref="headerRef">
        <template #btn>
          <common-button size="mini" @click="showType='addBtn';editFormVisible = true" icon="el-icon-plus" type="primary" class="filter-item">
            新增
          </common-button>
        </template>
      </mHeader>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
      <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="200px"
      />
      <el-table-column
        v-if="columns.visible('testingFeeTypeName')"
        align="center"
        key="testingFeeTypeName"
        prop="testingFeeTypeName"
        :show-overflow-tooltip="true"
        label="检测费类别"
        width="120px"
      />
      <el-table-column
        align="center"
        :key="item"
        prop="feeAmount"
        :show-overflow-tooltip="true"
        :label="item.toString() + '月'"
        v-for="item in monthArr"
      >
        <template v-slot="scope">
          <div v-if="scope.row.testingFeeList.findIndex((v) => v.month == item) > -1">
            <template v-for="value in scope.row.testingFeeList" :key="value">
              <template v-if="value.month == item">
                <span>{{ toThousand(value.feeAmount,decimalPrecision.contract) }}</span>
              </template>
            </template>
          </div>
          <template v-else>
            <span>/</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalAmount')"
        align="center"
        key="totalAmount"
        prop="totalAmount"
        :show-overflow-tooltip="true"
        label="合计"
      />
      <el-table-column align="center" label="操作" width="120px">
        <template v-slot="scope">
          <!-- <udOperation :data="scope.row" /> -->
          <common-button size="mini" type="primary" @click="toDetail(scope.row)" v-permission="permission.detail">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form :showType="showType" v-model:visible="editFormVisible" :info="itemInfo" @refresh="crud.toQuery" />
    <!-- 详情 -->
    <mDetail :detail-data="detailData" v-model:visible="drawerVisible" :query="crud.query" @refresh="crud.toQuery" />
  </div>
</template>

<script setup>
import { ref, provide } from 'vue'
import crudApi from '@/api/contract/expense-entry/testing-cost'

import { expenseTestingCostPM as permission } from '@/page-permission/contract'
import { toThousand } from '@data-type/number'
import useCRUD from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import useMaxHeight from '@compos/use-max-height'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import mHeader from './module/header.vue'
import mForm from './module/form.vue'
import mDetail from './module/detail.vue'

const { decimalPrecision } = useDecimalPrecision()

const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)
const dict = useDict(['testing_fee_type'])
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

const editFormVisible = ref(false)
const itemInfo = ref({})
const showType = ref('addBtn')

const { crud, CRUD, columns } = useCRUD(
  {
    title: '检测费',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    requiredQuery: ['year'],
    hasPagination: false
  },
  tableRef
)

provide('dict', dict)
provide('crud', crud)

const dataFormat = ref([
  ['project', 'parse-project'],
  ['totalAmount', ['to-thousand', decimalPrecision.contract]]
])

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 详情
function toDetail(row) {
  drawerVisible.value = true
  detailData.value = row.sourceRow
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.totalAmount = v.testingFeeList?.reduce((prev, curr) => {
      if (curr) {
        return prev + curr?.feeAmount
      } else {
        return prev
      }
    }, 0)
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>

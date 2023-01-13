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
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
      <el-table-column
        v-if="columns.visible('payDate')"
        align="center"
        key="payDate"
        prop="payDate"
        :show-overflow-tooltip="true"
        label="支付时间"
      >
        <template #default="{ row }">
          <span>{{ parseTime(row.payDate, '{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project.name')"
        align="center"
        key="project.name"
        prop="project.name"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="200px"
      />
      <el-table-column
        v-if="columns.visible('testingFeeTypeName') && crud.query.testingFeeTypeId !== undefined"
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
        :label="item.toString()"
        v-for="item in monthArr"
      >
        <template v-slot="scope">
          <div v-if="scope.row.testingFeeList.findIndex((v) => v.month == item) > -1">
            <template v-for="value in scope.row.testingFeeList" :key="value">
              <template v-if="value.month == item">
                <span>{{ value.feeAmount }}</span>
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
      >
        <template #default="{ row }">
          <span>{{ row.totalAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作" width="120px">
        <template v-slot="scope">
          <!-- <udOperation :data="scope.row" /> -->
          <common-button size="mini" type="primary" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form />
    <!-- 详情 -->
    <mDetail :detail-data="detailData" v-model:visible="drawerVisible" :query="crud.query" />
  </div>
</template>

<script setup>
import { ref, provide } from 'vue'
import { parseTime } from '@/utils/date'
import crudApi from '@/api/contract/expense-entry/testing-cost'
import useCRUD from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import useMaxHeight from '@compos/use-max-height'
// import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'
import mDetail from './module/detail.vue'

const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)
const dict = useDict(['testing_fee_type'])
const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i)
}
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '检测费',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    crudApi: { ...crudApi },
    requiredQuery: ['year'],
    invisibleColumns: ['payDate'],
    hasPagination: false
  },
  tableRef
)

provide('dict', dict)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// 详情
function toDetail(row) {
  drawerVisible.value = true
  detailData.value = row
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

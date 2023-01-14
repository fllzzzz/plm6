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
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('reimburseDate')"
        align="center"
        key="reimburseDate"
        prop="reimburseDate"
        :show-overflow-tooltip="true"
        label="日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reimburseDate ? parseTime(scope.row.reimburseDate, '{y}-{m}-{d}') : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('deptName')"
        align="center"
        key="deptName"
        prop="deptName"
        :show-overflow-tooltip="true"
        label="部门"
      >
        <template v-slot="scope">
          <span>{{ scope.row.deptName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reimburseUserName')"
        align="center"
        key="reimburseUserName"
        prop="reimburseUserName"
        :show-overflow-tooltip="true"
        label="报销人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reimburseUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('expenseTypeName')"
        align="center"
        key="expenseTypeName"
        prop="expenseTypeName"
        :show-overflow-tooltip="true"
        label="费用类别"
      >
        <template v-slot="scope">
          <span>{{ scope.row.expenseTypeName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('expenseSubjectName')"
        align="center"
        key="expenseSubjectName"
        prop="expenseSubjectName"
        :show-overflow-tooltip="true"
        label="费用科目"
      >
        <template v-slot="scope">
          <span>{{ scope.row.expenseSubjectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reimburseAmount')"
        align="center"
        key="reimburseAmount"
        prop="reimburseAmount"
        :show-overflow-tooltip="true"
        label="报销金额（元）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reimburseAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
      >
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('writtenByName')"
        align="center"
        key="writtenByName"
        prop="writtenByName"
        :show-overflow-tooltip="true"
        label="填报人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.writtenByName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作">
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 表单 -->
    <m-form />
  </div>
</template>
<script setup>
import crudApi, { getExpenseType } from '@/api/contract/expense-entry/expense-reporting'
import { ref, reactive, provide } from 'vue'

import { parseTime } from '@/utils/date'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { projectNameFormatter } from '@/utils/project'

import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const expenseList = ref([])
const summaryData = reactive({
  info: 0
})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '费用填报',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

initExpenseType()

provide('summaryData', summaryData)
provide('expenseList', expenseList.value)

async function initExpenseType() {
  try {
    const { content } = await getExpenseType()
    for (let i = 0; i < content.length; i++) {
      expenseList.value.push({
        id: content[i]?.id,
        name: content[i]?.name
      })
    }
  } catch (e) {
    console.log('获取费用类别失败', e)
  }
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  const values = []
  res.data.content = res.data.content?.map((v) => {
    values.push(v.reimburseAmount)
    return v
  })
  summaryData.info = values.reduce((prev, curr) => {
    const value = Number(curr)
    if (!isNaN(value)) {
      return prev + curr
    } else {
      return prev
    }
  }, 0)
}
</script>
<style lang="scss" scoped>
</style>

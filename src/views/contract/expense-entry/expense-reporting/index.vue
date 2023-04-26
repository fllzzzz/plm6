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
      :data-format="columnsDataFormat"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" :selectable="row => !row.isAmortization" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('reimburseDate')"
        align="center"
        key="reimburseDate"
        prop="reimburseDate"
        :show-overflow-tooltip="true"
        label="报销日期"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('deptName')"
        align="center"
        key="deptName"
        prop="deptName"
        :show-overflow-tooltip="true"
        label="部门"
      />
      <el-table-column
        v-if="columns.visible('reimburseUserName')"
        align="center"
        key="reimburseUserName"
        prop="reimburseUserName"
        :show-overflow-tooltip="true"
        label="报销人"
      />
      <el-table-column
        v-if="columns.visible('costAscriptionEnum')"
        align="center"
        key="costAscriptionEnum"
        prop="costAscriptionEnum"
        :show-overflow-tooltip="true"
        label="费用归属"
      />
      <el-table-column
        v-if="columns.visible('expenseTypeName')"
        align="center"
        key="expenseTypeName"
        prop="expenseTypeName"
        :show-overflow-tooltip="true"
        label="费用类别"
      />
      <el-table-column
        v-if="columns.visible('expenseSubjectName')"
        align="center"
        key="expenseSubjectName"
        prop="expenseSubjectName"
        :show-overflow-tooltip="true"
        label="费用科目"
      />
      <el-table-column
        v-if="columns.visible('reimburseAmount')"
        align="center"
        key="reimburseAmount"
        prop="reimburseAmount"
        :show-overflow-tooltip="true"
        label="报销金额（元）"
      />
      <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
      />
      <el-table-column
        v-if="columns.visible('writtenByName')"
        align="center"
        key="writtenByName"
        prop="writtenByName"
        :show-overflow-tooltip="true"
        label="填报人"
      />
      <el-table-column
        v-if="columns.visible('payee')"
        align="center"
        key="payee"
        prop="payee"
        :show-overflow-tooltip="true"
        label="收款单位"
      />
      <el-table-column
        v-if="columns.visible('remark')"
        align="center"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
      />
      <el-table-column align="center" label="操作">
        <template v-slot="{ row }">
          <el-tag v-if="row.isAmortization" size="medium" type="success" effect="plain"> 已摊销 </el-tag>
          <udOperation v-else :data="row" :permission="permission" />
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
import { ref, provide } from 'vue'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { expenseReportingPM as permission } from '@/page-permission/contract'
import { costAscriptionEnum } from '@enum-ms/config'

import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}
const tableRef = ref()
const expenseList = ref([])

const columnsDataFormat = ref([
  ['reimburseDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['costAscriptionEnum', ['parse-enum', costAscriptionEnum]]
])

const { crud, columns } = useCRUD(
  {
    title: '费用填报',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

initExpenseType()

provide('expenseList', expenseList)

async function initExpenseType() {
  try {
    const { content = [] } = await getExpenseType()
    expenseList.value = content
  } catch (e) {
    console.log('获取费用类别失败', e)
  }
}
const { maxHeight } = useMaxHeight({
  paginate: true
})

</script>

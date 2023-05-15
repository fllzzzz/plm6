<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.isProject"
        :options="projectReimbursementTypeEnum.ENUM"
        class="filter-item"
        type="enum"
        showOptionAll
        @change="crud.toQuery"
      />
      <time-range-select :query="query" clearable class="filter-item" style="width: 270px" @change="crud.toQuery" />
      <el-cascader
        :options="cascaderTree"
        v-model="cascaderValue"
        :props="cascaderProps"
        separator=" > "
        show-all-levels
        clearable
        size="small"
        class="filter-item"
        style="width: 260px"
        placeholder="可选费用归属/费用类型/费用科目"
        @change="cascaderChange"
      />
      <project-cascader v-model="query.projectId" clearable style="width: 260px" class="filter-item" @change="crud.toQuery" />
      <el-input
        v-model.trim="query.reimbursementPerson"
        clearable
        style="width: 150px"
        size="small"
        placeholder="报销人搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.payee"
        clearable
        style="width: 150px"
        size="small"
        placeholder="收款单位搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag effect="plain" class="filter-item" size="medium">
          <span> 报销总额（元）： <span v-thousand="totalAmount" /> </span>
        </el-tag>
        <print-table
          api-key="expenseReimburseList"
          :params="{
            ...query,
          }"
          size="mini"
          type="warning"
          v-permission="crud.permission.print"
        />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { summary } from '@/api/contract/expense-entry/expense-reporting'
import { ref, inject } from 'vue'

import { projectReimbursementTypeEnum } from '@enum-ms/contract'
import { isNotBlank } from '@/utils/data-type'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import projectCascader from '@comp-base/project-cascader'
import timeRangeSelect from '@comp-common/time-range-select/index'

const totalAmount = ref(0)
const cascaderValue = ref([])
const cascaderProps = ref({
  value: 'id',
  label: 'label',
  children: 'links',
  checkStrictly: true
})

const cascaderTree = inject('cascaderTree')

const defaultQuery = {
  expenseTypeId: undefined,
  expenseSubjectId: undefined,
  dateQueryTypeEnum: undefined,
  startDate: undefined,
  endDate: undefined,
  payee: undefined,
  projectId: undefined,
  isProject: undefined,
  costAttribution: undefined,
  reimbursementPerson: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)

function cascaderChange(val = []) {
  query.costAttribution = isNotBlank(val?.[0]) ? val[0] : undefined
  query.expenseTypeId = isNotBlank(val?.[1]) ? val[1] : undefined
  query.expenseSubjectIds = isNotBlank(val?.[2]) ? val[2] : undefined
  crud.toQuery()
}

async function getSummary() {
  try {
    totalAmount.value = (await summary(query)) || 0
  } catch (e) {
    console.log('获取费用填报汇总金额失败', e)
  }
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  getSummary()
}
</script>

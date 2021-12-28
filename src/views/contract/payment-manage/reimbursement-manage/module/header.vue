<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.dateType"
        :options="contractReimbursementDateEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="日期类型"
        style="width: 110px"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 200px"
      />
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <expense ref="expenseRef" v-model="query.choseId" @change="expenseChange" :clearable="true"   class="filter-item" placeholder="报销种类"/>
      <el-input v-model="query.applyDepartName" placeholder="申请部门" style="width: 120px" class="filter-item" />
      <el-input v-model="query.applyUserName" placeholder="申请人" style="width: 120px" class="filter-item" />
      <el-input v-model="query.writtenByName" placeholder="填报人" style="width: 120px" class="filter-item" />
      <common-select
        v-model="query.auditStatus"
        :options="reimbursementTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="状态"
        style="width: 120px"
        @change="crud.toQuery"
      />
      <rrOperation />
      <crudOperation add-text="报销填报">
        <template #viewLeft>
          <el-tag type="success" v-if="totalSum" size="medium">{{ `报销总额:${toThousand(totalSum)}元` }}</el-tag>
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { reimbursementTypeEnum, contractReimbursementDateEnum } from '@enum-ms/contract'
import { reimbursementSum } from '@/api/contract/supplier-manage/reimbursement'
import Expense from './expense'
import { toThousand } from '@data-type/number'

const defaultQuery = {
  projectId: undefined,
  dateType: contractReimbursementDateEnum.ENUM.UPDATE_DATE.V,
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  applyDepartName: undefined,
  applyUserName: undefined,
  type: undefined,
  choseId: undefined,
  writtenByName: undefined
}

const { CRUD, crud, query } = regHeader(defaultQuery)
const totalSum = ref()
getReimbursementSum()

async function getReimbursementSum() {
  let data
  try {
    data = await reimbursementSum()
  } catch (e) {
    console.log('报销总额', e)
  } finally {
    totalSum.value = data
  }
}

function expenseChange(val) {
  if (val) {
    if (val.type === 1) {
      crud.query.type = val.parentId
    } else {
      crud.query.type = val.id
    }
  } else {
    crud.query.type = undefined
  }
  crud.toQuery()
}

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime && crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  }
}
</script>

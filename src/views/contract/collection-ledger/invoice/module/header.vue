<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button
        :disabled="query.type === 2"
        size="small"
        :type="'all'"
        v-model="query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        :options="projectOrScrapEnum.ENUM"
        type="enum"
        v-model="query.type"
        class="filter-item"
        @change="crud.toQuery"
        showOptionAll
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
        style="width: 240px"
        @change="crud.toQuery"
      />
      <common-select
        v-if="query.type"
        v-model="query.businessType"
        :options="businessTypeEnum.ENUM"
        :unshowOptions="
          query.type === 1 ? [businessTypeEnum.SCRAPSELLER.K] :query.type === 2 ? [businessTypeEnum.INSTALLATION.K, businessTypeEnum.MACHINING.K] : []
        "
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="业务类型"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <div>
        <el-input v-model.trim="query.name" placeholder="项目名称" style="width: 200px" class="filter-item" />
        <el-input v-model.trim="query.contractSignBodyName" placeholder="合同签订主体" style="width: 200px" class="filter-item" />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="invoiceLedger"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <el-tag class="filter-item" size="medium"><span>开票合计：</span>{{ totalNumber }}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { businessTypeEnum, auditTypeEnum, projectOrScrapEnum } from '@enum-ms/contract'
import { getTotalNumber } from '@/api/contract/collection-and-invoice/invoice'

const defaultQuery = {
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  projectId: undefined,
  type: undefined,
  name: undefined,
  auditStatus: { value: auditTypeEnum.PASS.V, resetAble: false }
}

const totalNumber = ref()

async function fetchData() {
  const res = await getTotalNumber({ ...query, startDate: query.createTime[0], endDate: query.createTime[1] })
  totalNumber.value = res
}

const { crud, query } = regHeader(defaultQuery)

watch(
  [() => query.projectId, () => query.createTime, () => query.businessType, () => query.name, () => query.auditStatus],
  () => {
    fetchData()
  },
  { immediate: true }
)
</script>

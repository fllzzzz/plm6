<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.type"
        :options="newArrearsStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width:100px!important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="arrearsList"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { newArrearsStatusEnum } from '@enum-ms/finance'

const defaultQuery = {
  year: String(new Date().getFullYear()),
  projectId: undefined,
  type: newArrearsStatusEnum.ARREARS.V
}

const { crud, query } = regHeader(defaultQuery)

</script>

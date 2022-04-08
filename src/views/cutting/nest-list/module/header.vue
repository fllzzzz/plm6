<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <el-date-picker
            style="margin-right: 5px; width: 150px"
            v-model="query.importTime"
            type="date"
            size="small"
            class="filter-item"
            format="YYYY-MM-DD"
            value-format="YYYY-MM-DD"
            placeholder="请选择日期"
            @change="crud.toQuery"
          />
          <common-radio-button
          style="margin-right: 8px"
          class="filter-item"
          v-model="query.state"
          :options="NestingEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="crud.toQuery"
        />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { NestingEnum } from '@enum-ms/cutting'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'


const defaultQuery = {
  importTime: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>



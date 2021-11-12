<template>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker v-model="date" type="month" size="small" class="filter-item" placeholder="选择月" @change="crud.toQuery" />
        <common-radio-button
          v-model="query.type"
          :options="mesEnclosureTypeEnum.ENUM"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'

import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import moment from 'moment'

const defaultQuery = {}

const date = ref(new Date())

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeToQuery = () => {
  query.date = moment(date).valueOf()
}
</script>

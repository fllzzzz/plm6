<template>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="date"
          type="month"
          size="small"
          class="filter-item"
          placeholder="选择月"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import moment from 'moment'

const defaultQuery = {
}

const date = ref(new Date())

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeToQuery = () => {
  query.date = moment(date).valueOf()
}
</script>

<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="query.yearMonth"
          type="month"
          format="YYYY/MM"
          value-format="YYYY-MM"
          placeholder="请选择"
          @change="crud.toQuery"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'

// import moment from 'moment'

import crudOperation from '@crud/CRUD.operation'

// const defaultQuery = {
//   yearMonth: moment(new Date().getTime()).startOf('month').format('YYYY-MM')
// }
const defaultQuery = {
  yearMonth: undefined
}
const { crud, query } = regHeader(defaultQuery)

</script>

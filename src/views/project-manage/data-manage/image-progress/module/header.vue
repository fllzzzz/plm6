<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.yearMonth"
        type="month"
        format="YYYY/MM"
        value-format="YYYY-MM"
        placeholder="请选择"
        @change="crud.toQuery"
      />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'

import moment from 'moment'

const defaultQuery = {
  yearMonth: moment(new Date().getTime()).startOf('month').format('YYYY-MM')
}

const { crud, query } = regHeader(defaultQuery)

</script>

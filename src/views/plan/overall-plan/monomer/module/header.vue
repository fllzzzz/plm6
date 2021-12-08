<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入单体名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation/>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)

// export default {
//   components: { rrOperation, crudOperation },
//   mixins: [header(defaultQuery)],
//   inject: ['permission'],
//   props: {
//     projectId: {
//       type: [Number, String],
//       required: true
//     }
//   },
//   watch: {
//     projectId(val) {
//       this.query.projectId = val
//       this.crud.toQuery()
//     }
//   },
//   created() {
//     this.query.projectId = this.projectId
//   }
// }
</script>

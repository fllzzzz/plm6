<template>
  <div>
    <crudOperation :permission="permission">
      <el-tag slot="viewLeft">双击表格中的“行”，可更改表格默认模板</el-tag>
    </crudOperation>
  </div>
</template>

<script>
import { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  nmae: void 0,
  enabled: void 0
}
export default {
  components: { crudOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  data() {
    return {
    }
  },
  methods: {
  }
}
</script>

<template>
  <div class="head-container">
    <factory-tabs v-model:value="query.factoryId" @tab-click="crud.toQuery()" />
    <crudOperation :permission="permission" />
  </div>
</template>

<script>
import { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'
import factoryTabs from '@/views/components/base/factory-tabs'
const defaultQuery = {
}

export default {
  components: { crudOperation, factoryTabs },
  mixins: [header(defaultQuery)],
  inject: ['permission']
}
</script>

<template>
  <div class="head-container">
    <workshop-select
      v-model="query.workShopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 300px"
      class="filter-item"
      default
      @change="crud.toQuery"
    />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import workshopSelect from '@/components-system/bridge/workshop-select'

const defaultQuery = {
  workShopId: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>

<style>
</style>

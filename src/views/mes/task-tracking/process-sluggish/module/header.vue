<template>
  <div class="head-container">
    <workshop-select
      v-model="query.workShopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      default
      @change="workShopChange"
    />
    <common-radio-button
      v-model="query.searchProductType"
      :options="[processMaterialListTypeEnum.ARTIFACT, processMaterialListTypeEnum.MACHINE_PART]"
      class="filter-item"
      type="enum"
      @change="searchChange"
    />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import workshopSelect from '@comp-mes/workshop-select'

const defaultQuery = {
  workShopId: undefined,
  searchProductType: processMaterialListTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)
function searchChange() {
  crud.query.productType = undefined
  crud.toQuery()
}
function workShopChange() {
  crud.query.productionLineId = undefined
  crud.toQuery()
}
</script>

<style>
</style>

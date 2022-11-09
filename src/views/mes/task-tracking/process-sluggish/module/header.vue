<template>
  <div class="head-container">
    <workshop-select
      v-model="query.workShopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      default
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.productType"
      :options="[processMaterialListTypeEnum.ARTIFACT, processMaterialListTypeEnum.MACHINE_PART]"
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import workshopSelect from '@comp-mes/workshop-select'

const defaultQuery = {
  workShopId: undefined,
  productType: processMaterialListTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style>
</style>

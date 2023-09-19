<template>
  <div class="head-container">
    <workshop-select
      v-model="query.workShopId"
      placeholder="请选择车间"
      :workshop-type="workshopTypeEnum.BUILDING.V"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      default
      @change="crud.toQuery"
    />
    <common-radio-button
      type="enum"
      v-model="query.weightStatus"
      :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import workshopSelect from '@comp-mes/workshop-select'

const defaultQuery = {
  workShopId: undefined,
  weightStatus: weightTypeEnum.NET.V
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style>
</style>

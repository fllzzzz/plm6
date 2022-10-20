<template>
  <div class="head-container">
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workshopInfId"
        placeholder="请选择车间"
        :factory-id="query.factoryId"
        style="width: 270px"
        class="filter-item"
        defaultValue
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.productType"
        :options="[processMaterialListTypeEnum.ARTIFACT,processMaterialListTypeEnum.MACHINE_PART]"
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
  workshopInfId: undefined,
  productType: processMaterialListTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)

</script>

<style>

</style>

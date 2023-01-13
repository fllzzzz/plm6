<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button v-model="query.boolStatus" showOptionAll :options="enabledEnum.ENUM" class="filter-item" type="enum" @change="crud.toQuery" />
      <!-- <common-radio-button
        v-model="query.type"
        :options="depreciationTypeEnum.ENUM"
        class="filter-item"
        type="enum"
        @change="crud.toQuery"
      /> -->
      <el-input
        v-model.trim="query.name"
        clearable
        style="width: 150px"
        size="small"
        placeholder="输入名称搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
      <crudOperation />
    </div>
  </div>
</template>
<script setup>
import { regHeader } from '@compos/use-crud'
import { enabledEnum } from '@enum-ms/common'
// import { depreciationTypeEnum } from '@enum-ms/contract'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  boolStatus: enabledEnum.TRUE.V,
  // type: depreciationTypeEnum.PLANT.V,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style lang="scss" scoped>
</style>

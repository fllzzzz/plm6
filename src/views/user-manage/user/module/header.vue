<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.blurry"
        size="small"
        placeholder="输入名称、手机号或者邮箱搜索"
        style="width: 260px;"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <common-radio-button
        v-model="query.enabled"
        :options="enabledEnum.ENUM"
        type="enum"
        showOptionAll
        class="filter-item"
        size="small"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { enabledEnum } from '@enum-ms/common'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  blurry: undefined,
  deptId: undefined,
  enabled: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style lang="scss" scoped>
.head-container {
  height: 82px;
  margin-left: 20px;
}
</style>

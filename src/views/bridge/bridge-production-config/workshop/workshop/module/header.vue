<template>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.boolEnabledEnum"
          :options="enabledEnum.ENUM"
          type="enum"
          showOptionAll
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-input
          v-model.trim="query.name"
          placeholder="输入车间名称搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation/>
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { enabledEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined,
  boolEnabledEnum: enabledEnum.TRUE.V
}

const { crud, query } = regHeader(defaultQuery)
</script>

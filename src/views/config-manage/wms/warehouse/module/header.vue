<template>
  <div class="head-container">
    <factory-tabs v-model="query.factoryId" @tab-click="handleTabClick" />
    <template v-if="query.factoryId">
      <crud-operation :disabled="!query.factoryId">
        <template #viewLeft>
          <common-radio-button
            v-model="query.enabled"
            :options="enabledEnum.ENUM"
            show-option-all
            type="enum"
            size="mini"
            @change="crud.toQuery"
          />
        </template>
      </crud-operation>
    </template>
  </div>
</template>

<script setup>
import { enabledEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import FactoryTabs from '@comp-base/factory-tabs.vue'
import CrudOperation from '@crud/CRUD.operation.vue'
const defaultQuery = {
  factoryId: undefined, // 工厂
  enabled: undefined // 使用状态
}

const { crud, query } = regHeader(defaultQuery)

function handleTabClick(data) {
  crud.props.factory = data
  crud.toQuery()
}
</script>

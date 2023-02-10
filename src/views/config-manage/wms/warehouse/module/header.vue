<template>
  <div class="head-container">
    <workshop-tabs v-model="query.workshopId" @tab-click="handleTabClick" />
    <template v-if="query.workshopId">
      <crud-operation :disabled="!query.workshopId">
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
import WorkshopTabs from '@comp-base/workshop-tabs.vue'
import CrudOperation from '@crud/CRUD.operation.vue'
const defaultQuery = {
  workshopId: undefined, // 车间
  enabled: undefined // 使用状态
}

const { crud, query } = regHeader(defaultQuery)

function handleTabClick(data) {
  crud.props.workshop = data
  crud.toQuery()
}
</script>

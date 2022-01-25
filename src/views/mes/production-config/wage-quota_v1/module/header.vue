<template>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.sequenceType"
          :options="typeEnum.ENUM"
          class="filter-item"
          type="enum"
          size="small"
          @change="sequenceTypeChange"
        />
        <common-radio-button
          v-if="query.sequenceType === typeEnum.ARTIFACT.V"
          v-model="query.processType"
          :options="processTypeEnum.ENUM"
          size="small"
          class="filter-item"
          type="enum"
          @change="crud.toQuery"
        />
        <el-input
          v-model="query.name"
          placeholder="输入名称搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { processTypeEnum, processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined,
  sequenceType: typeEnum.ARTIFACT.V,
  processType: processTypeEnum.ONCE.V
}

const { crud, query } = regHeader(defaultQuery)

function sequenceTypeChange() {
  if (query.sequenceType === typeEnum.ARTIFACT.V) {
    query.processType = processTypeEnum.ONCE.V
  } else {
    delete query.processType
  }
  crud.toQuery()
}
</script>

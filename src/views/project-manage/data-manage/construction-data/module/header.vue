<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.constructionStageType"
        :options="constructionEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <common-button type="warning" size="mini" @click="emit('handleUpload')" v-permission="crud.permission.import" class="filter-item">
          新增
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

import { constructionEnum } from '@enum-ms/project'

const defaultQuery = {
  constructionStageType: undefined // 类型 1开工前 2施工中 4竣工后
}

const emit = defineEmits(['handleUpload'])
const { crud, query } = regHeader(defaultQuery)

</script>

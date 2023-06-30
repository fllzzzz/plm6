<template>
  <div class="head-container">
      <common-radio-button
        v-model="query.workshopId"
        :options="workshopName"
        showOptionAll
        type="other"
        :data-structure="{ key: 'id', label: 'name', value: 'id' }"
        class="filter-item"
        @change="workshopNameChange"
      />
      <crud-operation :disabled="!query.workshopId">
        <template #optLeft>
          <common-button icon="el-icon-plus" type="success" class="filter-item" size="mini" @click="workshopVisible=true">仓库名称</common-button>
        </template>
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
      <workshopList v-model="workshopVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import useWorkshopName from '@compos/store/use-workshop-name'

import { enabledEnum } from '@enum-ms/common'
import { isNotBlank } from '@data-type/index'
import { warehouseTypeEnum } from '@enum-ms/wms'
import { regHeader } from '@compos/use-crud'
import CrudOperation from '@crud/CRUD.operation.vue'
import workshopList from './workshop-list'

const defaultQuery = {
  workshopId: undefined, // 车间
  enabled: undefined // 使用状态
}

const { crud, query } = regHeader(defaultQuery)

const workshopVisible = ref(false)

const { workshopName } = useWorkshopName()

function workshopNameChange(val) {
  const findVal = workshopName.value.find(v => v.id === val) || {}
  crud.query.warehouseType = isNotBlank(findVal) ? (findVal.workshopId ? warehouseTypeEnum.WORKSHOP.V : warehouseTypeEnum.NORMAL.V) : undefined
  crud.toQuery()
}
</script>

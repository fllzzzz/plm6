<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.printType"
        :options="boolPrintedEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <!-- <common-radio-button
        v-if="crud.query.processType === mesMachinePartOrderTypeEnum.CUTTING_ORDER.V"
        v-model="query.cutConfigId"
        :options="cutTypeList"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        showOptionAll
        type="other"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <crudOperation>
        <template #optLeft>
          <workshop-select
            v-model="query.workshopId"
            placeholder="请选择车间"
            :workshop-type="workshopTypeEnum.BUILDING.V"
            :factory-id="query.factoryId"
            style="width: 200px"
            class="filter-item"
            defaultValue
            @change="crud.toQuery"
          />
          <el-input
            v-model="query.cutNumber"
            placeholder="指令号搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
            重置
          </common-button>
        </template>
      </crudOperation>
    </div>
  </div>
</template>
<script setup>
import { inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import { boolPrintedEnum, workshopTypeEnum } from '@enum-ms/common'
import workshopSelect from '@/components-system/base/workshop-select.vue'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  factoryId: undefined,
  workshopId: undefined,
  printType: undefined,
  cutNumber: undefined,
  cutConfigId: undefined
}

const cutTypeList = inject('cutTypeList')
const { crud, query } = regHeader(defaultQuery)

function searchQuery() {
  crud.toQuery()
}
function resetQuery() {
  query.factoryId = undefined
  query.workshopId = undefined
  query.printType = undefined
  query.cutNumber = undefined
  query.cutConfigId = undefined
  crud.toQuery()
}
</script>

<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">           
           <factory-select  
           v-model="query.factoryId" 
           placeholder="请选择工厂" 
           class="filter-item" 
           style="width: 270px"
           :factory-id="query.factoryId"/>
           <!-- @change="crud.toQuery"  -->
           
          <workshop-select
          v-model="query.workshopInfId"
          placeholder="请先选择车间"
          style="width: 270px"
          class="filter-item"
          :factory-id="query.factoryId"
          />
          <production-line-select
          v-model="query.productionLineId"
          :factory-id="query.factoryId"
          placeholder="请先选择生产线"
          class="filter-item"
          style="width: 270px"/>
          <!-- <rrOperation /> -->
          <common-button class="filter-item" size="mini" type="primary">确定</common-button>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
// import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'

const defaultQuery = {
  factoryId:undefined,
  workshopInfId:undefined,
}

const { crud, query } = regHeader(defaultQuery)
</script>

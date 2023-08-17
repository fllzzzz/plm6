<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <el-input
          v-model="query.name"
          size="small"
          placeholder="输入名称搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          size="small"
          placeholder="输入编号搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.plate"
          size="small"
          placeholder="输入板型搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.material"
          size="small"
          placeholder="输入材质搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input-number
          v-model.number="query.minLength"
          :min="0"
          :max="query.maxLength || 999999999"
          :precision="0"
          :controls="false"
          size="small"
          style="width: 130px"
          class="filter-item"
          clearable
          placeholder="最小长度(mm)"
          @keyup.enter="crud.toQuery"
        />
        <span class="filter-item">~</span>
        <el-input-number
          v-model.number="query.maxLength"
          class="filter-item"
          :min="query.minLength || 0"
          :precision="0"
          :max="999999999"
          :controls="false"
          size="small"
          style="width: 130px"
          clearable
          placeholder="最大长度(mm)"
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: void 0,
  minLength: void 0,
  maxLength: void 0,
  serialNumber: void 0,
  plate: void 0,
  material: void 0
}

const { crud, query } = regHeader(defaultQuery)
</script>

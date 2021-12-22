<template>
  <div>
    <div v-show="crud.searchToggle">
        <el-input
        v-model="query.name"
        placeholder="按岗位名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <el-radio-group v-model="query.enabled" size="small" class="filter-item"  @change="crud.toQuery">
        <el-radio-button
          v-for="item in systemEnabledEnum.ENUM"
          :key="item.V"
          :label="item.V"
        >
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { systemEnabledEnum } from '@enum-ms/system'
import { ElRadioGroup } from 'element-plus'

const defaultQuery = {
  name: undefined,
  enabled: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>

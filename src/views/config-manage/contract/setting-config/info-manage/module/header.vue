<template>
  <div>
    <crudOperation>
      <template #viewLeft>
        <el-tag size="medium">双击表格中的“行”，可更改表格默认模板</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>

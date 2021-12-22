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
import crudOperation from '@crud/CRUD.operation'

</script>

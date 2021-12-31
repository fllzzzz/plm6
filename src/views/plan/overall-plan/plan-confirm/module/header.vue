<template>
  <div>
    <crudOperation>
      <template #optLeft>
        <div v-loading="crud.loading">
          <el-tag type="success" class="tag-style">总计划100</el-tag>
          <el-tag type="success" class="tag-style">按期完成100</el-tag>
          <el-tag type="success" class="tag-style">未确认100</el-tag>
          <el-tag type="success" class="tag-style">已确认100</el-tag>
          <el-tag type="danger" class="tag-style" style="margin-right:0;">已超期100</el-tag>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
}
const { crud } = regHeader(defaultQuery)

</script>
<style lang="scss" scoped>
.tag-style{
  width:180px;
  height:60px;
  font-size:18px;
  line-height:60px;
  text-align:center;
  margin-right:20px;
}
</style>

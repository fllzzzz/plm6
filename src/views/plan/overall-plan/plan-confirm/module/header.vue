<template>
  <div>
    <el-row :gutter="30" v-loading="crud.loading">
      <el-col :span="4">
        <el-card shadow="hover" :body-style="{ background: '#f0f9eb', color:'#67c23a' }"> 总计划100 </el-card>
      </el-col>
      <el-col :span="4">
        <el-card shadow="hover" :body-style="{ background: '#f0f9eb', color:'#67c23a' }"> 按期完成100 </el-card>
      </el-col>
      <el-col :span="4">
        <el-card shadow="hover" :body-style="{ background: '#f0f9eb', color:'#67c23a' }"> 未确认100 </el-card>
      </el-col>
      <el-col :span="4">
        <el-card shadow="hover" :body-style="{ background: '#f0f9eb', color:'#67c23a' }"> 已确认100 </el-card>
      </el-col>
      <el-col :span="4">
        <el-card class="danger-card" shadow="hover" :body-style="{ background: '#fef0f0', color:'#f56c6c' }"> 已超期100 </el-card>
      </el-col>
    </el-row>
    <crudOperation />
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
::v-deep(.el-card){
  border-color:#e1f3d8;
}
::v-deep(.danger-card){
  border-color:#fde2e2;
}
</style>

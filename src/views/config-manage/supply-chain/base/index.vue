<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <subcontractConfig/>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import subcontractConfig from './component/subcontract-config/index'

</script>

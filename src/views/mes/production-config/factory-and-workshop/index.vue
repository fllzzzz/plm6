<template>
  <div class="app-container">
    <el-row :gutter="10">
      <!-- 工厂列表 -->
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12" style="margin-bottom:10px;">
        <factory-config @click-factory="handleChangeFactory" />
      </el-col>
      <!-- 车间列表 -->
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12">
        <workshop-config :factory="curFactory" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import factoryConfig from './factory/index'
import workshopConfig from './workshop/index'

const curFactory = ref()

function handleChangeFactory(val) {
  curFactory.value = val
}
</script>

<template>
  <div class="app-container">
    <div class="head-container">
      <common-button icon="el-icon-plus" type="success" @click="addScrap">新增</common-button>
    </div>
    <common-table>
      <el-table-column label="序号" align="center" width="80" />
      <el-table-column label="废料类型" align="center"></el-table-column>
      <el-table-column label="单位" align="center"></el-table-column>
      <el-table-column label="操作" align="center">
        <template #default>
          <common-button type="success">修改</common-button>
          <common-button type="danger">删除</common-button>
        </template>
      </el-table-column>
    </common-table>
    <detaile v-model="detaileVisible" />
  </div>
</template>
<script setup>
import { onMounted, ref } from 'vue'
import { getScrapConfig } from '@/api/config/wms/scrap-config'
import detaile from './module/detaile.vue'

const detaileVisible = ref(false)

onMounted(() => {
  fetchData()
})

const fetchData = async () => {
  try {
    const { content } = await getScrapConfig()
    console.log(content)
  } catch (error) {
    console.log(error)
  }
}

const addScrap = () => {
  detaileVisible.value = true
}

</script>

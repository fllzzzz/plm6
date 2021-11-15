<template>
  <div class="notify-component">
    <el-badge :value="total" :max="99" :hidden="total < 1" class="item">
      <svg-icon icon-class="notify" @click="visible = true" />
    </el-badge>
    <table-list v-model="visible" />
  </div>
</template>

<script setup>
import { fetchInventoryNotify } from '@/api/wms/common'
import { ref, onBeforeUnmount } from 'vue'
import tableList from './table.vue'

const visible = ref(false)
const total = ref(0)

onBeforeUnmount(() => {
  clearInterval(timer)
})

getTotal()

// 轮询
const timer = setInterval(() => {
  getTotal()
}, 60000)

// 获取数量
async function getTotal() {
  try {
    const { totalElements = 0 } = await fetchInventoryNotify()
    total.value = totalElements
  } catch (error) {
    console.log('获取库存预警列表失败', error)
  }
}

</script>
<style>
.notify-component .el-badge__content.is-fixed {
  top: 6px;
  right: -10px;
  transform: scale(.7);
}
</style>

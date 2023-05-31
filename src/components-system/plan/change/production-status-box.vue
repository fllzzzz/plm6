<template>
  <div style="display: flex">
    <div v-for="item in processList" :key="item.name" class="process-item">
      <span
        class="name"
        :style="
          ['未生产', '套料'].includes(item.name)
            ? 'background-color: #36ae81;'
            : ['入库', '发运', '装配'].includes(item.name)
            ? 'background-color: #2179eb;'
            : ''
        "
      >
        {{ item.name }}
      </span>
      <span>{{ processObj[item.name].quantity }}</span>
    </div>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

defineProps({
  processList: {
    type: Array,
    default: () => []
  },
  processObj: {
    type: Object,
    default: () => {}
  }
})
</script>

<style lang="scss" scoped>
.process-item {
  display: flex;
  flex-direction: column;
  align-items: center;
  box-shadow: rgba(255, 255, 255, 0) 0px 0px 1px;
  border: 1px solid #eff0f2;
  border-radius: 5px;
  margin-right: 5px;
  background: #fff;
  overflow: hidden;
  min-width: 50px;

  .name {
    width: 100%;
    background: #eb7421;
    color: #fff;
    padding: 1px 5px;
    font-size: 10px;
  }
}
</style>

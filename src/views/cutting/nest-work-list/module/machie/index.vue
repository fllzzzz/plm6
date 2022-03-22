
<template>
  <div class="production-lines-content">
    <el-tag
      v-for="item in machineData"
      :effect="selectLineId === item.id ? 'light' : 'plain'"
      :type="selectLineId === item.id ? 'success' : 'info'"
      class="elTag"
      hit
      @click="handleChange(item)"
      :key="item.id"
      >{{ item.machineName }}</el-tag
    >
  </div>
</template>

<script setup>
// import { inject, defineProps, defineEmits } from 'vue'
import { defineProps, defineEmits } from 'vue'

const emit = defineEmits(['change'])

// const productType = inject('productType')

defineProps({
  machineData: {
    type: Array,
    default: () => []
  },
  selectLineId: {
    type: Number,
    default: undefined
  },
  isSingle: {
    type: Boolean,
    default: false
  }
})

function handleChange(item) {
  emit('change', item)
}

</script>

<style lang="scss" scoped>
.production-lines-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  box-sizing: border-box;
  padding: 10px 0;
  .el-tag {
    width: 200px;
    max-width: 300px;
    text-align: center;
    margin: 0 15px 15px 0;
    cursor: pointer;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
  }
  .el-tag--info {
    border-color: #303133;
    color: #303133;
  }
}
.elTag {
  width: 200px;
  max-width: 300px;
  text-align: center;
  margin: 0 15px 15px 0;
  cursor: pointer;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
}
</style>

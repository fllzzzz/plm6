<template>
  <div v-for="workshop in lines" :key="workshop.id">
    <div class="workshop-content" v-if="workshop.productionLineList && workshop.productionLineList.length">
      <div class="workshop-title">{{ workshop.name }}【{{ workshop.factoryName }}】</div>
      <div class="production-lines-content">
        <template v-for="line in workshop.productionLineList" :key="line.id">
          <el-tag
            hit
            v-if="productType && productType === line.productType"
            :effect="isSingle ? (selectLineId === line.id ? 'light' : 'plain') : line.selected ? 'light' : 'plain'"
            :type="isSingle ? (selectLineId === line.id ? 'success' : 'info') : line.selected ? 'success' : 'info'"
            @click="handleChange(workshop, line)"
            >{{ line.name }}</el-tag
          >
        </template>
      </div>
    </div>
  </div>
</template>

<script setup>
import { inject, defineProps, defineEmits } from 'vue'

const emit = defineEmits(['change'])

const productType = inject('productType')

defineProps({
  lines: {
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

function handleChange(workshop, line) {
  emit('change', { workshop, line })
}
</script>

<style lang="scss" scoped>
.workshop-content {
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  align-items: flex-start;
  width: 100%;
  margin-bottom: 20px;

  .workshop-title {
    // color: #b54e4e;
    color: #1c75c6;
    font-weight: bold;
  }

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
}
</style>

<template>
  <el-input
    v-model.trim="queryVO.name"
    size="small"
    placeholder="输入名称搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.serialNumber"
    size="small"
    placeholder="输入编号搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.specification"
    size="small"
    placeholder="输入规格搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
  <el-input
    v-if="showMaterial"
    v-model.trim="queryVO.material"
    size="small"
    placeholder="输入材质搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'

const emit = defineEmits(['to-query'])

const props = defineProps({
  productType: {
    type: Number
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  },
  // 围护子类型
  category: {
    type: Number
  },
  showMaterial: {
    type: Boolean,
    default: true
  }
})

const queryVO = ref({})
watchEffect(() => {
  queryVO.value = props.query
})

function toQuery() {
  emit('to-query')
}
</script>

<template>
  <el-form-item label="物料">
    <span v-empty="{ val: material.classifyFullName }" />
  </el-form-item>
  <el-form-item v-if="material.specificationLabels" label="规格">
    <el-tooltip :content="material.specificationLabels" :disabled="!material.specificationLabels" placement="top">
      <span v-empty="{ val: material.specification }" />
    </el-tooltip>
  </el-form-item>
  <slot name="afterSpec" />
  <el-form-item v-if="material.color" label="颜色">
    <span v-empty="{ val: material.color }" />
  </el-form-item>
  <el-form-item label="品牌">
    <span v-empty="{ val: material.brand }" />
  </el-form-item>
  <slot name="afterBrand" />
  <el-form-item v-if="material.project" label="项目">
    <span v-parse-project="{ project: material.project, onlyShortName: true }" v-empty-text />
  </el-form-item>
  <el-form-item label="仓库">
    <span v-empty="{ val: warehouseName }" />
  </el-form-item>
</template>

<script setup>
import { defineProps, computed } from 'vue'

const props = defineProps({
  material: {
    // 物料信息
    type: Object
  }
})

const warehouseName = computed(() => {
  const fcName = props.material.factory ? props.material.factory.name : ''
  const warehouseName = props.material.warehouse ? props.material.warehouse.name : ''
  return `${fcName} - ${warehouseName}`
})
</script>

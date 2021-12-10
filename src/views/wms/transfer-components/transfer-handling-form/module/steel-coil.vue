<template>
  <common-material-info :material="material">
    <template #afterSpec>
      <el-form-item label="厚 * 宽">
        <span>{{ `${material.thickness}mm * ${material.width}mm` }}</span>
      </el-form-item>
    </template>
    <template #afterBrand>
      <el-form-item label="炉批号">
        <span>{{ material.heatNoAndBatchNo }}</span>
      </el-form-item>
    </template>
  </common-material-info>
</template>

<script setup>
import { defineProps } from 'vue'

import commonMaterialInfo from '../components/common-material-info.vue'

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  material: {
    // 物料出库信息
    type: Object
  }
})
</script>

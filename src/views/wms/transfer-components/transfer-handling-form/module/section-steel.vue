<template>
  <template v-if="loaded">
    <common-material-info :material="material">
      <template #afterSpec>
        <el-form-item label="定尺长度">
          <span>{{ `${material.length}${baseUnit.length.unit}` }}</span>
        </el-form-item>
      </template>
      <template #afterBrand>
        <el-form-item label="炉批号">
          <span>{{ material.heatNoAndBatchNo }}</span>
        </el-form-item>
      </template>
    </common-material-info>
  </template>
</template>

<script setup>
import { defineProps } from 'vue'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import commonMaterialInfo from '../components/common-material-info.vue'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料信息
    type: Object
  }
})
// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit(props.basicClass)
</script>

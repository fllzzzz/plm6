<template>
  <el-descriptions border :column="4">
    <el-descriptions-item label="物料">
      <template #label>
        <span>物料</span>
        <el-tag v-if="material.boolPartyA" type="danger" style="margin-left: 10px">甲供</el-tag>
      </template>
      <span v-empty="{ val: material.classifyFullName }" />
    </el-descriptions-item>
    <el-descriptions-item v-if="material.specificationLabels" label="规格">
      <el-tooltip :content="material.specificationLabels" :disabled="!material.specificationLabels" placement="top">
        <span v-empty="{ val: material.specification }" />
      </el-tooltip>
    </el-descriptions-item>
    <slot name="afterSpec" />
    <el-descriptions-item v-if="material.color" label="颜色">
      <span v-empty="{ val: material.color }" />
    </el-descriptions-item>
    <el-descriptions-item label="品牌">
      <span v-empty="{ val: material.brand }" />
    </el-descriptions-item>
    <slot name="afterBrand" />
    <template v-if="material.project">
      <el-descriptions-item label="项目">
        <span v-parse-project="{ project: material.project, onlyShortName: true }" v-empty-text />
      </el-descriptions-item>
      <el-descriptions-item label="单体">
        <span v-empty="{ val: material.monomerName }" />
      </el-descriptions-item>
      <el-descriptions-item label="区域">
        <span v-empty="{ val: material.areaName }" />
      </el-descriptions-item>
    </template>
    <el-descriptions-item label="仓库">
      <span v-empty="{ val: warehouseName }" />
    </el-descriptions-item>
    <el-descriptions-item label="可出库/库存">
      <span style="color: green" v-to-fixed="{ val: maxQuantity, dp: material.outboundUnitPrecision }" />
      /
      <span v-to-fixed="{ val: material.corQuantity, dp: material.outboundUnitPrecision }" />
      <span style="margin-left: 10px">{{ material.outboundUnit }}</span>
    </el-descriptions-item>
    <el-descriptions-item label="库存重量" v-if="material.basicClass & STEEL_ENUM">
      <span style="color: green;" v-to-fixed="{ val: material.operableMete, dp: material.accountingPrecision }" />
      /
      <span v-to-fixed="{ val: material.mete, dp: material.accountingPrecision }" />
      <span style="margin-left: 10px">{{ material.accountingUnit }}</span>
    </el-descriptions-item>
  </el-descriptions>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { STEEL_ENUM } from '@/settings/config'

const props = defineProps({
  material: {
    // 物料出库信息
    type: Object
  },
  form: {
    // 物料出库信息
    type: Object
  }
})
const maxQuantity = inject('maxQuantity')

const warehouseName = computed(() => {
  const fcName = props.material.factory ? props.material.factory.name : ''
  const warehouseName = props.material.warehouse ? props.material.warehouse.name : ''
  return `${fcName} - ${warehouseName}`
})
</script>

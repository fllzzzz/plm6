<template>
  <el-table-column
    v-if="showMonomer"
    prop="monomer.name"
    label="单体"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
    min-width="120px"
  />
  <el-table-column v-if="showArea" prop="area.name" label="区域" align="center" :fixed="fixed" show-overflow-tooltip min-width="120px" />
  <el-table-column v-if="showName" prop="name" label="名称" align="center" :fixed="fixed" show-overflow-tooltip min-width="100px" />
  <el-table-column
    v-if="showSerialNumber"
    prop="serialNumber"
    label="编号"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
    min-width="100px"
  />
  <el-table-column
    v-if="showSpecification"
    prop="specification"
    label="规格"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
    min-width="140px"
  />
  <el-table-column v-if="showLength" prop="length" label="长度（mm）" align="center" :fixed="fixed" show-overflow-tooltip />
  <el-table-column v-if="showMaterial" prop="material" label="材质" align="center" :fixed="fixed" show-overflow-tooltip />
  <el-table-column v-if="showQuantity" prop="quantity" label="数量" align="center" :fixed="fixed" show-overflow-tooltip />
  <el-table-column v-if="showMete" prop="mete" label="总重(kg)" align="center" :fixed="fixed" show-overflow-tooltip />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  }
})

const showMonomer = computed(() => isBlank(props.columns) || props.columns.visible('monomer.name'))
const showArea = computed(() => isBlank(props.columns) || props.columns.visible('area.name'))
const showName = computed(() => isBlank(props.columns) || props.columns.visible('name'))
const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible('serialNumber'))
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showLength = computed(() => isBlank(props.columns) || props.columns.visible('length'))
const showMaterial = computed(() => isBlank(props.columns) || props.columns.visible('material'))
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible('quantity'))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible('mete'))
</script>

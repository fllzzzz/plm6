<template>
  <div class="head-container">
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      needConvert
      clearable
      @change="handleMonomerChange"
    />
    <el-input
      v-model.trim="query.serialNumber"
      size="small"
      placeholder="输入编号搜索"
      style="width: 170px"
      class="filter-item"
      clearable
      @change="handleSerialNumberChange"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const emit = defineEmits(['change'])
const defaultQuery = {
  projectId: undefined,
  monomerId: undefined,
  areaId: undefined,
  serialNumber: undefined
}

const { query } = regHeader(defaultQuery)

function handleMonomerChange(val) {
  emit('change', val)
}
function handleSerialNumberChange(val) {
  emit('change', val)
}
</script>

<style>
</style>

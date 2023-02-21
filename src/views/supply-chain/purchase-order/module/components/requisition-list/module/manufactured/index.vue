<template>
  <div class="head-container">
    <common-radio-button
      type="enum"
      v-model="basicClass"
      :options="manufClsEnum.ENUM"
      :disabledVal="[manufClsEnum.ENCL_MANUFACTURED.V]"
      clearable
      class="filter-item"
    />
    <span style="float: right">
      <common-button type="warning" class="filter-item" size="mini" icon="el-icon-plus" @click="add">选择加入</common-button>
    </span>
  </div>
  <common-table v-bind="$attrs" :data="list" return-source-data @selection-change="handleSelectionChange">
    <el-table-column type="selection" width="55" align="center" :selectable="selectable" />
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="quantity" label="申购数量" align="center" show-overflow-tooltip />
    <el-table-column prop="totalNetWeight" label="总重（kg）" align="center" show-overflow-tooltip />
  </common-table>
</template>

<script setup>
import { defineExpose, defineEmits, inject, ref } from 'vue'

import { manufClsEnum } from '@enum-ms/classification'
import { isBlank, toPrecision } from '@/utils/data-type'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['add-purchase'])
const form = inject('crud')?.form
const basicClass = ref(manufClsEnum.STRUC_MANUFACTURED.V)
const list = ref([])
const selectList = ref([])

function selectable(row) {
  return isBlank(form.requisitionListKV?.[row.id])
}

function initList(_list) {
  list.value = _list.map((v) => {
    v.totalNetWeight = toPrecision(v.quantity * v.netWeight, 2)
    v.basicClass = manufClsEnum.STRUC_MANUFACTURED.V
    return v
  })
}

function handleSelectionChange(val) {
  selectList.value = val
}

function add() {
  if (!selectList.value?.length) {
    ElMessage.warning('请选择数据')
    return
  }
  selectList.value.forEach((v) => {
    emit('add-purchase', v)
  })
}

defineExpose({
  initList
})
</script>

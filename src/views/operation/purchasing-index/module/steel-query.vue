<template>
  <basic-class-classification
    ref="classificationRef"
    class="filter-item"
    v-model="query.classifyId"
    :basic-class="matClsEnum.STEEL_PLATE.V"
    placeholder="请选择钢板类"
    @change="handleSubjectIdChange"
  ></basic-class-classification>
  <template v-if="query.classifyId">
    <el-select
      v-model="query.thickness"
      clearable
      placeholder="请选择厚度"
      class="filter-item"
      filterable
      style="width: 130px"
      @change="handleQueryChange"
    >
      <el-option v-for="item in thickness" :key="item" :label="item" :value="item" />
    </el-select>
    <template v-for="item in currenMat.specifications" :key="item.id">
      <el-select
        v-if="item.name === '材质'"
        clearable
        v-model="query.specId"
        filterable
        :placeholder="`请选择${item.name}`"
        class="filter-item"
        @change="handleQueryChange"
      >
        <el-option v-for="spec in item.list" :key="spec.id" :label="spec.value" :value="spec.id" />
      </el-select>
    </template>
  </template>
</template>

<script setup>
import { getInboundThicknessById } from '@/api/wms/common'
import { inject, ref, defineEmits } from 'vue'

import { matClsEnum } from '@enum-ms/classification'

import basicClassClassification from './basic-class-classification.vue'

const emit = defineEmits(['toQuery'])
const query = inject('steel-query', {})
const classificationRef = ref()
const currenMat = ref({})
const thickness = ref([])

function handleSubjectIdChange(id) {
  console.log(id)
  query.thickness = undefined
  query.specId = undefined
  handleQueryChange()
  fetchThickness(id)
}

function handleQueryChange() {
  emit('toQuery')
}

async function fetchThickness(id) {
  if (!id) return
  try {
    thickness.value = []
    currenMat.value = {}
    const data = await getInboundThicknessById(id)
    thickness.value = data
    currenMat.value = classificationRef.value.getOption(id)
  } catch (error) {
    console.log(error, '获取已入库钢板的厚度列表')
  }
}
</script>

<style lang="scss" scoped></style>

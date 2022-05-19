<template>
  <basic-class-classification
    ref="classificationRef"
    class="filter-item"
    v-model="query.classifyId"
    :basic-class="matClsEnum.SECTION_STEEL.V"
    placeholder="请选择型材类"
    @change="handleSubjectIdChange"
  ></basic-class-classification>
  <template v-if="query.classifyId">
    <el-select
      v-model="query.sectionSteelSpecId"
      clearable
      filterable
      placeholder="请选择规格"
      class="filter-item"
      @change="handleQueryChange"
    >
      <el-option v-for="item in specList" :key="item.id" :label="item.name" :value="item.id" />
    </el-select>
    <template v-for="item in currenMat.specifications" :key="item.id">
      <el-select
        v-if="item.name === '材质'"
        clearable
        filterable
        v-model="query.specId"
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
import { getFinalMatClsById } from '@/api/config/classification-manage/common'
import { inject, ref, defineEmits } from 'vue'

import { matClsEnum } from '@enum-ms/classification'

import basicClassClassification from './basic-class-classification.vue'

const emit = defineEmits(['toQuery'])
const query = inject('section-query', {})
const classificationRef = ref()
const currenMat = ref({})
const specList = ref([])

function handleSubjectIdChange(id) {
  console.log(id)
  query.sectionSteelSpecId = undefined
  query.specId = undefined
  handleQueryChange()
  fetchFinalMatCls(id)
}

function handleQueryChange() {
  emit('toQuery')
}

async function fetchFinalMatCls(id) {
  if (!id) return
  try {
    specList.value = []
    currenMat.value = {}
    const data = await getFinalMatClsById(id)
    const { list = [] } = data?.nationalStandard.find((v) => v.boolDefault)
    specList.value = list
    currenMat.value = classificationRef.value.getOption(id)
  } catch (error) {
    console.log(error, '获取型材的规格')
  }
}
</script>

<style lang="scss" scoped></style>

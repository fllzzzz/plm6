<template>
  <div class="head-container">
    <common-radio-button
      type="enum"
      v-model="basicClass"
      :options="steelClsEnum.ENUM"
      :disabledVal="disabledVal"
      clearable
      class="filter-item"
    />
  </div>
  <component ref="compRef" v-if="basicClass" :is="comp" v-bind="$attrs" />
</template>

<script setup>
import { ref, reactive, computed, provide, watch, defineExpose } from 'vue'
import { steelClsEnum } from '@enum-ms/classification'

import { deepClone } from '@data-type/index'
import SteelPlateTable from './module/steel-plate-table'
import SectionSteelTable from './module/section-steel-table'
import SteelCoilTable from './module/steel-coil-table'

const compRef = ref()
const basicClass = ref(steelClsEnum.STEEL_PLATE.V)
const bcListKV = {
  [steelClsEnum.STEEL_PLATE.K]: 'steelPlateList',
  [steelClsEnum.SECTION_STEEL.K]: 'sectionSteelList',
  [steelClsEnum.STEEL_COIL.K]: 'steelCoilList'
}
const bcListObj = reactive({
  steelPlateList: [],
  sectionSteelList: [],
  steelCoilList: []
})
provide('bcListObj', bcListObj)

const disabledVal = computed(() => {
  const _val = []
  for (const item in steelClsEnum.ENUM) {
    if (!bcListObj[bcListKV[item]]?.length) {
      _val.push(steelClsEnum[item].V)
    }
  }
  return _val.length === Object.keys(steelClsEnum.ENUM)?.length ? [] : _val
})

const comp = computed(() => {
  switch (basicClass.value) {
    case steelClsEnum.STEEL_PLATE.V:
      return SteelPlateTable
    case steelClsEnum.SECTION_STEEL.V:
      return SectionSteelTable
    case steelClsEnum.STEEL_COIL.V:
      return SteelCoilTable
    default:
      return ''
  }
})

watch(
  () => compRef,
  () => {
    if (compRef.value) {
      bcListObj[bcListKV[steelClsEnum.VK[basicClass.value]]].forEach((v) => {
        compRef.value.rowWatch(v)
      })
    }
  },
  { immediate: true, deep: true }
)

function initList(list) {
  bcListObj.sectionSteelList = []
  bcListObj.steelPlateList = []
  bcListObj.steelCoilList = []
  if (!list?.length) {
    basicClass.value = steelClsEnum.STEEL_PLATE.V
  } else {
    list.forEach((v) => {
      v.weighingTotalWeight = v.mete
      bcListObj[bcListKV[steelClsEnum.VK[v.basicClass]]].push(deepClone(v))
    })
    for (const item in steelClsEnum.ENUM) {
      if (bcListObj[bcListKV[item]]?.length) {
        basicClass.value = steelClsEnum[item].V
        break
      }
    }
  }
}

defineExpose({
  initList
})
</script>

<style lang="scss" scoped></style>

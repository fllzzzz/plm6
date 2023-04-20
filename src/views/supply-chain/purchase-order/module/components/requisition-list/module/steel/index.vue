<template>
  <div class="head-container">
    <common-radio-button
      type="enum"
      v-model="basicClass"
      :options="steelClsEnum.ENUM"
      :disabledVal="disabledVal"
      clearable
      class="filter-item"
    >
      <template #suffix="{ item }">
        <span v-if="bcListObj[bcListKV[item.K]]?.length">({{ bcListObj[bcListKV[item.K]].length }})</span>
      </template>
    </common-radio-button>
  </div>
  <component ref="compRef" v-if="basicClass" :is="comp" v-bind="$attrs" />
</template>

<script setup>
import { ref, reactive, computed, provide, watch, defineExpose } from 'vue'
import { steelClsEnum } from '@enum-ms/classification'

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
      addRowWatch()
    }
  },
  { immediate: true, deep: true }
)

function addRowWatch() {
  bcListObj[bcListKV[steelClsEnum.VK[basicClass.value]]].forEach((v) => {
    compRef.value.rowWatch(v)
  })
}

function initList(list) {
  bcListObj.sectionSteelList = []
  bcListObj.steelPlateList = []
  bcListObj.steelCoilList = []
  basicClass.value = steelClsEnum.STEEL_PLATE.V
  if (!list?.length) return
  let _basicClass = steelClsEnum.STEEL_PLATE.V
  list.forEach((v, i) => {
    // v.weighingTotalWeight = v.mete
    bcListObj[bcListKV[steelClsEnum.VK[v.basicClass]]].push(reactive(v))
    _basicClass = v.basicClass < _basicClass || i === 0 ? v.basicClass : _basicClass
  })
  basicClass.value = _basicClass
  addRowWatch()
}

defineExpose({
  initList
})
</script>

<style lang="scss" scoped></style>

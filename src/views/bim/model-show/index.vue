<template>
  <div class="app-container">
    <div class="head-container" style="margin-bottom: 0px">
      <div class="filter-container">
        <div class="filter-left-box">
          <monomer-select
            ref="monomerRef"
            v-model="query.monomerId"
            :project-id="globalProjectId"
            default
            placeholder="可选择单体"
            class="filter-item"
            style="width: 200px"
            @getAreaInfo="getAreaInfo"
            @getCurrentInfo="getCurrentInfo"
          />
          <common-select
            v-if="importMode === modelImportModeEnum.INTEGRATION.V"
            v-model="query.areaId"
            :options="areaList"
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            clearable
            class="filter-item"
            placeholder="请选择区域"
            style="width: 200px"
          ></common-select>
        </div>
      </div>
    </div>
    <bim-Model-view
      :monomerId="query.monomerId"
      :areaId="query.areaId"
      :maxHeight="maxHeight"
      :monomerName="monomerName"
      :projectName="globalProject?.shortName"
    />
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'
import { mapGetters } from '@/store/lib'
// import store from '@/store'

import { modelImportModeEnum } from '@enum-ms/bim'
import useMaxHeight from '@compos/use-max-height'

import bimModelView from '@/components-system/bim/bim-model-view'
import monomerSelect from '@/components-system/plan/monomer-select'

const monomerRef = ref()
const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])
const query = ref({})
const { maxHeight } = useMaxHeight({})
const monomerName = computed(() => {
  return monomerRef.value?.getOption(query.value.monomerId)?.name
})
// onBeforeMount(() => {
//   store.dispatch('app/closeSideBar', { withoutAnimation: false })
// })
// onUnmounted(() => {
//   store.dispatch('app/toggleSideBar')
// })
const areaList = ref([])
const importMode = ref()

function getAreaInfo(val) {
  areaList.value = val || []
}
function getCurrentInfo({ bimConfig }) {
  importMode.value = bimConfig?.importMode
}
</script>

<style lang="scss" scoped></style>

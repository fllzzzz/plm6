<template>
  <div class="app-container">
    <div class="head-container" style="margin-bottom:0px;">
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
          />
        </div>
      </div>
    </div>
    <bim-Model-view
      :monomerId="query.monomerId"
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
</script>

<style lang="scss" scoped></style>

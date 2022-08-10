<template>
  <bim-Model-view :showMonomerModel="false" :projectId="globalProjectId" :projectName="globalProject?.shortName" :maxHeight="maxHeight" />
</template>

<script setup>
// import { onBeforeMount, onUnmounted } from 'vue'
import { mapGetters } from '@/store/lib'
// import store from '@/store'

import useMaxHeight from '@compos/use-max-height'

import bimModelView from '@/components-system/bim/bim-model-view'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])
const { maxHeight } = useMaxHeight({})
// onBeforeMount(() => {
//   store.dispatch('app/closeSideBar', { withoutAnimation: false })
// })
// onUnmounted(() => {
//   store.dispatch('app/toggleSideBar')
// })
</script>

<style lang="scss" scoped></style>

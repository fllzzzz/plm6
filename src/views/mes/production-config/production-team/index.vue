<template>
  <component :is="currentView" :productionLineTypeEnum="productionLineTypeEnum">
    <template #teamType>
      <common-radio-button
        v-model="productionLineTypeEnum"
        :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
        type="enum"
        size="small"
        class="filter-item"
        @change="handleLineTypeChange"
      />
      <common-radio-button
        class="filter-item"
        v-model="teamType"
        :unshowVal="productionLineTypeEnum & artifactProductLineEnum.INTELLECT.V ? [teamTypeEnum.INSPECTION.V] : []"
        size="small"
        type="enum"
        :options="teamTypeEnum.ENUM"
      />
    </template>
  </component>
</template>

<script setup>
import { ref, computed } from 'vue'
import { artifactProductLineEnum, traditionLineEnum } from '@enum-ms/mes'
import { teamTypeEnum } from '@enum-ms/mes'
import team from './team'
import inspection from './inspection'
import { mapGetters } from '@/store/lib'

const teamType = ref(teamTypeEnum.TEAM.V)
const productionLineTypeEnum = ref(artifactProductLineEnum.TRADITION.V)

const { hasIntelligent } = mapGetters('hasIntelligent')
const currentView = computed(() => {
  return teamType.value & teamTypeEnum.TEAM.V ? team : inspection
})

function handleLineTypeChange() {
  if (productionLineTypeEnum.value & artifactProductLineEnum.INTELLECT.V) {
    teamType.value = teamTypeEnum.TEAM.V
  }
}
</script>

<style lang="scss" scoped></style>

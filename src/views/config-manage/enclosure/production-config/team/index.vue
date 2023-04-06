<template>
  <component :is="currentView">
    <template #teamType>
      <common-radio-button
        class="filter-item"
        v-model="teamType"
        size="small"
        type="enum"
        :options="teamTypeEnum.ENUM"
      />
    </template>
  </component>
</template>

<script setup>
import { ref, computed } from 'vue'
import { teamTypeEnum } from '@enum-ms/mes'
import team from './team'
import inspection from './inspection'

const teamType = ref(teamTypeEnum.TEAM.V)

const currentView = computed(() => {
  return teamType.value & teamTypeEnum.TEAM.V ? team : inspection
})
</script>

<template>
  <team-wage >
    <template #header>
      <mHeader />
    </template>
  </team-wage>
</template>

<script setup>
import { provide } from 'vue'
import { artifactTeamWagePM as permission } from '@/page-permission/mes'
import teamWage from '../components/team-wage/index.vue'
import mHeader from './module/header.vue'

provide('name', '围护')
provide('permission', permission)
</script>

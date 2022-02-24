<template>
  <team-wage-header></team-wage-header>
</template>

<script setup>
import { provide } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import teamWageHeader from '../../components/team-wage/header'

provide('initProductType', componentTypeEnum.ENCLOSURE.V)
</script>

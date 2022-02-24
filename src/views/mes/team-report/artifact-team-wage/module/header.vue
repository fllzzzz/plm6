<template>
  <team-wage-header></team-wage-header>
</template>

<script setup>
import { provide } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import teamWageHeader from '../../components/team-wage/header'

const productTypeOptionAllValue = componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V | componentTypeEnum.ARTIFACT.V

provide('initProductType', productTypeOptionAllValue)
</script>

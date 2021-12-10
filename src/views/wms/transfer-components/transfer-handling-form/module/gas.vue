<template>
  <common-material-info :material="material" />
</template>

<script setup>
import { defineProps } from 'vue'
import commonMaterialInfo from '../components/common-material-info.vue'

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  material: {
    // 物料出库信息
    type: Object
  }
})
</script>

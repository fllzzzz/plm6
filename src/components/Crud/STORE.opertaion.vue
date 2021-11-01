<template>
  <span style="display: inline-block;margin-left: 6px;">
    <common-button size="mini" @click="fmStore.saveStoreForm" type="warning" plain>存为草稿</common-button>
    <common-button size="mini" @click="fmStore.resetForm" type="info" plain>清空</common-button>
  </span>
</template>

<script setup>
import { inject } from 'vue'
const fmStore = inject('fmStore')
</script>

<template>
  <!-- <span style="display: inline-block;margin-left: 6px;"> -->
    <common-button size="mini" @click="fmStore.saveStoreForm" type="warning" plain>存为草稿</common-button>
    <common-button size="mini" @click="fmStore.resetForm" type="info" plain>清空</common-button>
  <!-- </span> -->
</template>

<script setup>
import { defineProps, inject } from 'vue'

const props = defineProps({
  type: {
    type: String,
    default: 'crud'
  }
})

let injectContent
switch (props.type) {
  case 'crud': injectContent = 'fmStore'
    break
  case 'crudBatch': injectContent = 'bfmStore'
    break
  case 'normal': injectContent = 'nfmStore'
    break
  default: injectContent = 'fmStore'
    break
}

const fmStore = inject(injectContent)
</script>

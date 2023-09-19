<template>
  <span v-if="fmStore" style="display: inline-block; margin-left: 6px">
    <common-button size="mini" @click="handleSave" type="warning" plain>存为草稿</common-button>
    <common-button size="mini" @click="handleClear" type="info" plain>清空</common-button>
  </span>
</template>

<script setup>
import { ElMessage } from 'element-plus'
import { defineProps, defineEmits, inject } from 'vue'

const emit = defineEmits(['clear'])

const props = defineProps({
  type: {
    type: String,
    default: 'crud'
  }
})

let injectContent
switch (props.type) {
  case 'crud':
    injectContent = 'fmStore'
    break
  case 'crudBatch':
    injectContent = 'bfmStore'
    break
  case 'cu':
    injectContent = 'cuFmStore'
    break
  case 'normal':
    injectContent = 'nfmStore'
    break
  default:
    injectContent = 'fmStore'
    break
}

const fmStore = inject(injectContent)

function handleSave() {
  const res = fmStore.saveStoreForm()
  if (res) ElMessage.success('已存为草稿')
}

function handleClear() {
  fmStore.resetForm()
  emit('clear')
}
</script>

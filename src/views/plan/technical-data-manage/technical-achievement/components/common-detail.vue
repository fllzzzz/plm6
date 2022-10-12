<template>
    <common-drawer
      append-to-body
      v-model="visible"
      top="10vh"
      :before-close="handleClose"
      :title="props.title"
      :wrapper-closable="false"
      size="80%"
    >
      <template #titleAfter>
        <el-tag v-if="currentRow.monomerName" size="medium" effect="plain">{{ currentRow.monomerName }}</el-tag>
      </template>
      <template #content>
        <detail :current-row="props.currentRow" :permission="props.permission" :upload-type="props.uploadType" :tip="props.tip" @success="emit('success')"/>
      </template>
    </common-drawer>
  </template>

<script setup>
import { defineProps, defineEmits } from 'vue'

import useVisible from '@compos/use-visible'
import detail from './match-detail'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: false
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  title: {
    type: String,
    default: '文件匹配详情'
  },
  uploadType: {
    type: String,
    default: '.pdf'
  },
  tip: {
    type: String,
    default: undefined
  }
})

const emit = defineEmits(['success', 'update:modelValue'])

const { visible, handleClose } = useVisible({ emit, props })

</script>


<template>
  <common-drawer ref="drawerRef" title="清单变更" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight> </template>
    <template #content> </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function showHook() {}
</script>

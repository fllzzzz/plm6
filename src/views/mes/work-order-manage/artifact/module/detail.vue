<template>
  <common-drawer ref="drawerRef" title="生产任务单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight> </template>
    <template #content> </template>
  </common-drawer>
</template>

<script setup>
import { productTask } from '@/api/mes/work-order-manage/artifact.js'
import { defineProps, defineEmits, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
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

const tableData = ref([])
const tableLoading = ref(false)

function showHook() {
  fetch()
}

async function fetch() {
  try {
    tableLoading.value = true
    await productTask({

    })
  } catch (err) {
    console.log('获取生产任务单', err)
  } finally {
    tableLoading.value = false
  }
}
</script>

<template>
  <div>
    <drawing-img v-if="!isPdf" v-model="dialogVisible" :fullscreen="fullscreen" :serial-number="serialNumber" />
    <drawing-pdf v-if="isPdf" v-model="dialogVisible" :fullscreen="fullscreen" :serial-number="serialNumber" />
  </div>
</template>

<script setup>
import { ElNotification } from 'element-plus'
import { computed, defineEmits, defineProps, ref } from 'vue'
import { previewPDF } from '@/api/plan/technical-data-manage/deepen'

import useVisible from '@compos/use-visible'

import drawingImg from './drawing-img'
import drawingPdf from './drawing-pdf'

const emit = defineEmits(['update:visible'])
const { visible: dialogVisible } = useVisible({ emit, props, field: 'visible', showHook: checkSuffix })

const props = defineProps({
  visible: {
    type: Boolean,
    required: false
  },
  fullscreen: {
    type: Boolean,
    default: true
  },
  serialNumber: {
    // 编号
    type: String,
    default: undefined
  },
  productId: {
    type: Number,
    default: undefined
  },
  productType: {
    type: Number,
    default: undefined
  }
})

const suffix = ref()

const isPdf = computed(() => {
  return suffix.value === 'pdf'
})

async function checkSuffix() {
  try {
    const param = {
      productId: props.productId,
      productType: props.productType
    }
    const res = await previewPDF(param)
    // 获取文件格式
    if (res.headers['content-disposition']) {
      suffix.value = res.headers['content-disposition'].split('=')[1].split('.')[1]
    } else {
      ElNotification({ title: '获取图纸失败，请确认是否上传图纸', type: 'error', duration: 3000 })
    }
  } catch (error) {
    console.log('获取图纸', error)
  }
}
</script>

<template>
  <div>
    <drawing-img ref="imgRef" v-if="!isPdf" v-model="ImgShow" :fullscreen="fullscreen" :serial-number="serialNumber" :showType="showType" :id="id" @close="emit('close')"/>
    <drawing-pdf ref="pdfRef" v-if="isPdf" v-model="PdfShow" :fullscreen="fullscreen" :serial-number="serialNumber" :showType="showType" :id="id"  @close="emit('close')"/>
  </div>
</template>

<script setup>
import { ElNotification } from 'element-plus'
import { computed, defineProps, ref, watch, defineEmits } from 'vue'
import { downloadAttachment } from '@/api/common'
import { previewPDF } from '@/api/plan/technical-data-manage/deepen'

import drawingImg from './drawing-img'
import drawingPdf from './drawing-pdf'
const props = defineProps({
  isVisible: {
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
  },
  showType: { // 展示类型，为图纸或附件
    type: String,
    default: undefined
  },
  id: {
    type: Number,
    default: undefined
  }
})

const suffix = ref()
const pdfRef = ref()
const imgRef = ref()
const emit = defineEmits(['close'])
const isPdf = computed(() => {
  return suffix.value === 'pdf'
})

const ImgShow = ref(false)
const PdfShow = ref(false)

watch(
  () => props.isVisible,
  (value) => {
    if (value) {
      checkSuffix()
    }
  },
  { immediate: true }
)
async function checkSuffix() {
  try {
    const param = {
      productId: props.productId,
      productType: props.productType
    }
    console.log(param)
    const res = props.showType ? await downloadAttachment({ id: props.id }) : await previewPDF(param)
    const msg = props.showType ? '附件' : '图纸'
    // 获取文件格式
    if (res.headers['content-disposition']) {
      const fileNameArr = res.headers['content-disposition'].split('=')[1].split('.')
      suffix.value = fileNameArr[fileNameArr.length - 1]
      ImgShow.value = suffix.value !== 'pdf'
      PdfShow.value = suffix.value === 'pdf'
    } else {
      ElNotification({ title: `获取${msg}失败，请确认是否已上传${msg}`, type: 'error', duration: 3000 })
    }
  } catch (error) {
    console.log('获取图纸', error)
  }
}
</script>

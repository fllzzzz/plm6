<template>
  <div id="pdfBox" ref="pdfBox" class="pdf-box">
    <pdf :url="source" :scale="scale" :rotation="viewRotate" @pdf-error="pdfError" :type="'canvas'" :pdfjsDistPath="pdfjsDistPath" />
  </div>
</template>

<script setup>
import { previewPDF } from '@/api/plan/technical-data-manage/deepen'

import { ElNotification } from 'element-plus'
import { defineEmits, defineExpose, inject, ref } from 'vue'
import pdf from '@/components/PDF/pdf'

const emit = defineEmits(['changeFileLoading'])
const productId = inject('productId')
const productType = inject('productType')
const pdfjsDistPath = import.meta.env.BASE_URL + 'assets'
const source = ref()
const scale = ref(1)
const viewRotate = ref(0)

function changeFileLoading(state) {
  emit('changeFileLoading', state)
}

function pdfError(error) {
  ElNotification({ title: '加载图纸失败，请确认是否已上传该编号图纸', type: 'error', duration: 3000 })
  console.error(error)
}

async function fetchDrawing() {
  // 设置滚动条高度为0
  const el = document.getElementById('pdfBox')
  if (el) {
    el.scrollTop = 0
  }
  try {
    changeFileLoading(true)
    const param = {
      productId: productId.value,
      productType: productType.value
    }
    const res = await previewPDF(param)
    if (res.headers['content-disposition']) {
      const suffix = res.headers['content-disposition'].split('=')[1].split('.')[1]
      if (suffix !== 'pdf') throw new Error('非.pdf格式')
    } else {
      throw new Error('获取文件格式失败')
    }
    source.value = await getUrlByFileReader(res)
    // 处理图纸
  } catch (error) {
    console.log('获取图纸', error)
    ElNotification({ title: '获取图纸失败', type: 'error', duration: 2000 })
  } finally {
    changeFileLoading(false)
  }
}

function getUrlByFileReader(res) {
  return new Promise((resolve, reject) => {
    if (res && res.data && res.data.size) {
      const dataInfo = res.data
      const reader = new window.FileReader()
      // 使用readAsArrayBuffer读取文件, result属性中将包含一个 ArrayBuffer 对象以表示所读取文件的数据
      reader.readAsArrayBuffer(dataInfo)
      reader.onload = function (e) {
        console.log(e, dataInfo, 'rrrr')
        const result = e.target.result
        const contentType = dataInfo.type
        // 生成blob图片,需要参数(字节数组, 文件类型)
        const blob = new Blob([result], { type: contentType })
        // 使用 Blob 创建一个指向类型化数组的URL, URL.createObjectURL是new Blob文件的方法,可以生成一个普通的url,可以直接使用,比如用在img.src上
        const url = window.URL.createObjectURL(blob)
        console.log(url) // 产生一个类似 blob:d3958f5c-0777-0845-9dcf-2cb28783acaf 这样的URL字符串
        resolve(url)
      }
    } else {
      reject()
    }
  })
}

function scaleZoom() {
  scale.value += 0.2
}
function scaleZoomOut() {
  scale.value -= 0.2
}
function clockwiseRotate() {
  viewRotate.value += 90
}
function counterclockwiseRotate() {
  viewRotate.value -= 90
}
function reset() {
  viewRotate.value = 0
  scale.value = 1
}
function setScale(val) {
  scale.value = val
}

defineExpose({
  fetchDrawing,
  reset,
  setScale,
  scaleZoom,
  scaleZoomOut,
  clockwiseRotate,
  counterclockwiseRotate
})
</script>

<style lang="scss" scoped></style>

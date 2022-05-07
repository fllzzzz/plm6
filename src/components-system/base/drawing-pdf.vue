<!-- 图纸pdf预览 -->
<template>
  <el-dialog
    title="图纸预览"
    v-model="dialogVisible"
    width="80%"
    :fullscreen="fullscreen"
    :custom-class="'drawing-pdf-dialog'"
    :before-close="handleClose"
  >
    <div v-show="!loading" class="root-content">
      <div class="pdf-content" @dblclick="showOperate = true" @click="showOperate = false">
        <div id="pdfBox" ref="pdfBox" class="pdf-box" @mousedown="move">
          <!-- <object :data="src" type="application/pdf" width="100%" height="900px" /> -->
          <!-- <embed :src="src" type="application/pdf" width="100%" height="700px"> -->
          <!-- <iframe :src="src" width="100%" height="500px" content-type="application/adobe-pdf" /> -->
          <pdf :url="source" :scale="scale" :rotation="viewRotate" @pdf-error="pdfError" :type="'canvas'" :pdfjsDistPath="pdfjsDistPath" />
        </div>
      </div>
      <div v-show="showOperate && !showType" class="operate-content">
        <div class="operate-left">{{ serialNumber }}</div>
        <div class="operate-middle">{{ `${pageNum} / ${pageTotalNum}` }}</div>
        <div class="operate-right" />
      </div>
      <div class="quick-operation">
        <div class="icon-box" @click="scaleZoom">
          <svg-icon class="icon" icon-class="comp-zoom" />
        </div>
        <div class="icon-box" @click="scaleZoomOut">
          <svg-icon class="icon" icon-class="comp-zoom-out" />
        </div>
        <div class="icon-box" @click="clockwiseRotate">
          <svg-icon class="icon" icon-class="comp-clockwise-rotate" />
        </div>
        <div class="icon-box" @click="counterclockwiseRotate">
          <svg-icon class="icon" icon-class="comp-counterclockwise-rotate" />
        </div>
        <div class="icon-box" @click="reset">
          <svg-icon class="icon" icon-class="comp-restore-size" />
        </div>
        <div class="icon-box" @click="handleClose">
          <svg-icon class="icon" icon-class="comp-quit" />
        </div>
      </div>
    </div>
    <div v-if="fileLoading || loading" class="load-box">
      <i class="el-icon-loading" />
    </div>
  </el-dialog>
</template>

<script setup>
import pdf from '@/components/PDF/pdf'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, ref, watch } from 'vue'
import { downloadAttachment } from '@/api/common'
import { previewPDF } from '@/api/plan/technical-data-manage/deepen'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:modelValue'])
const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
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

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'modelValue' })

const pdfjsDistPath = import.meta.env.BASE_URL + 'assets'
const source = ref()
const scale = ref(1)
const viewRotate = ref(0)
const showOperate = ref(false)
const pageNum = ref()
const pageTotalNum = ref()
const loading = ref(false)
const fileLoading = ref(false)

watch(
  () => props.modelValue,
  (value) => {
    if (value) {
      dialogVisible.value = true
      fetch()
    }
  },
  { immediate: true }
)

async function fetch() {
  // if (!dialogVisible.value) {
  //   return
  // }
  init()
  const msg = props.showType ? '附件' : '图纸'
  try {
    fileLoading.value = true
    const param = {
      productId: props.productId,
      productType: props.productType
    }
    const res = props.showType ? await downloadAttachment({ id: props.id }) : await previewPDF(param)
    source.value = await getUrlByFileReader(res)
    // 处理图纸
  } catch (error) {
    console.log(`获取${msg}`, error)
    handleClose()
    ElNotification({ title: `获取${msg}失败`, type: 'error', duration: 2000 })
  } finally {
    fileLoading.value = false
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
        console.log(e, dataInfo)
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

function init() {
  pageNum.value = 1
  pageTotalNum.value = 1
  // 设置滚动条高度为0
  const el = document.getElementById('pdfBox')
  if (el) {
    el.scrollTop = 0
  }
  reset()
}

function pdfError(error) {
  const msg = props.showType ? '附件' : '图纸'
  ElNotification({ title: `加载${msg}失败，请确认是否已上传该${msg}`, type: 'error', duration: 3000 })
  console.error(error)
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
</script>

<style lang="scss">
@import '@/styles/index.scss';

.drawing-pdf-dialog {
  background: #525659;
  .el-dialog__header {
    display: none;
  }
  .el-dialog__body {
    padding: 0;
  }
  .root-content {
    position: relative;
  }
  .load-box {
    @extend .flex-rcc;
    position: absolute;
    top: 0;
    right: 0;
    width: 100%;
    height: 100%;
    font-size: 100px;
    color: white;
  }
  .pdf-content {
    box-sizing: border-box;
    padding: 0 50px;
    @extend .flex-rcc;
    width: 100%;
    height: 100vh;
  }
  .pdf-box {
    width: 100%;
    height: 100vh;
    overflow: auto;
    span {
      width: 100%;
      margin: 0 auto;
    }
  }
}

.quick-operation {
  position: absolute;
  bottom: 15vh;
  right: 0vh;
  width: 50px;
  height: 450px;
  @extend .flex-cac;
  .icon-box:nth-child(n) {
    position: relative;
    @extend .flex-rcc;
    width: 35px;
    height: 35px;
    :hover {
      width: 80%;
      height: 80%;
    }
  }
  .icon-box {
    cursor: pointer;
    background: rgb(83, 83, 82);
    // background: black;
    border-radius: 50%;
    opacity: 0.5;
    &:hover {
      opacity: 1;
      // background: rgb(83, 83, 82);
      // border-radius: 50%;
    }
  }
  .icon {
    position: absolute;
    width: 25px;
    height: 25px;
  }
}

.operate-content {
  position: absolute;
  top: 0;
  height: 50px;
  width: 100%;
  box-sizing: border-box;
  padding: 0 100px;
  background: #323639;
  color: white;
  @extend .flex-rbc;
  .operate-middle {
    @extend .flex-rsc;
  }
  .operate-right {
    @extend .flex-rsc;
    .icon-box:nth-child(n) {
      position: relative;
      @extend .flex-rcc;
      width: 35px;
      height: 35px;
      margin-left: 15px;
      :hover {
        width: 80%;
        height: 80%;
      }
    }
    .icon-box {
      cursor: pointer;
      &:hover {
        background: rgb(83, 83, 82);
        border-radius: 50%;
      }
    }
  }
  // .icon {
  //     position: absolute;
  //     width: 25px;
  //     height: 25px;
  // }
}
</style>

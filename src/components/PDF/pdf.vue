<template>
  <div class="pdf-view">
    <div id="viewerContainer" ref="containerRef">
      <div id="viewer" class="pdfViewer" />
    </div>
  </div>
</template>

<script setup>
import { defineProps, onMounted, ref, watch, defineEmits } from 'vue'
import getPdfjsDist from './getPdfjsDist'
import { isNotBlank, isBlank } from '@data-type/index'

const emit = defineEmits(['pdf-error'])
const props = defineProps({
  url: {
    type: String,
    default: ''
  },
  scale: {
    type: Number,
    default: 1
  },
  rotation: {
    type: Number,
    default: 0
  },
  type: {
    type: String,
    default: 'canvas'
  },
  pdfjsDistPath: {
    type: String,
    default: '.'
  }
})

const containerRef = ref()
const pdfViewer = ref()
const pdfLinkService = ref()
const currentScale = ref('page-actual')
const loadingTask = ref()
const CMAP_URL = ref()

onMounted(() => {
  pdfLibInit().then(() => {
    renderPdf()
  })
})

watch(
  () => props.url,
  () => {
    // 如果存在pdfViewer则取消渲染
    if (pdfViewer.value) {
      pdfViewer.value._cancelRendering()
    }
    renderPdf()
  }
)

watch(
  () => props.scale,
  () => {
    console.log(pdfViewer.value)
    pdfViewer.value.currentScaleValue = props.scale
    pdfLinkService.value.setViewer(pdfViewer.value)
  }
)

watch(
  () => props.rotation,
  () => {
    pdfViewer.value.pagesRotation = props.rotation
    pdfLinkService.value.setViewer(pdfViewer.value)
  }
)

function onPagesInit({ source }) {
  source.currentScaleValue = currentScale
}

async function pdfLibInit() {
  let pdfjsLib = window.pdfjsLib
  let pdfjsViewer = window.pdfjsViewer
  if (!pdfjsLib || !pdfjsViewer) {
    try {
      await getPdfjsDist(props.pdfjsDistPath)
      CMAP_URL.value = `${props.pdfjsDistPath}/pdf/cmaps/`
      pdfjsLib = window.pdfjsLib
      pdfjsLib.GlobalWorkerOptions.workerSrc = `${props.pdfjsDistPath}/pdf/build/pdf.worker.js`
      pdfjsViewer = window.pdfjsViewer
    } catch (error) {
      console.log(error, 'pdfjs文件获取失败')
      // pdfjs文件获取失败
      return
    }
  }
  const container = containerRef.value
  const eventBus = new pdfjsViewer.EventBus()

  const _pdfLinkService = new pdfjsViewer.PDFLinkService({
    eventBus: eventBus
  })
  pdfLinkService.value = _pdfLinkService
  const _pdfViewer = new pdfjsViewer.PDFViewer({
    container: container,
    eventBus: eventBus,
    linkService: pdfLinkService.value,
    renderer: props.type,
    textLayerMode: 0,
    downloadManager: new pdfjsViewer.DownloadManager({}),
    enableWebGL: true
  })
  pdfViewer.value = _pdfViewer
  pdfLinkService.value.setViewer(pdfViewer.value)

  eventBus.on('pagesinit', onPagesInit)
}
function renderPdf() {
  if (!props.url) {
    return
  }
  // Loading document.
  if (isBlank(pdfViewer.value) || isBlank(pdfLinkService.value)) {
    return
  }
  if (isNotBlank(loadingTask.value)) {
    loadingTask.value.destroy()
    loadingTask.value = null
  }
  loadingTask.value = window.pdfjsLib.getDocument({
    cMapUrl: CMAP_URL.value,
    cMapPacked: true,
    url: props.url
  })
  return loadingTask.value.promise
    .then((pdfDocument) => {
      if (pdfDocument.loadingTask.destroyed || !props.url) {
        return
      }
      pdfViewer.value.setDocument(pdfDocument)
      pdfLinkService.value.setDocument(pdfDocument, null)
      loadingTask.value = null
    })
    .catch((error) => {
      emit('pdf-error', error)
    })
}
</script>

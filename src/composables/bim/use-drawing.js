import { createApp, ref } from 'vue'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

export default function useDrawing() {
  const drawingApp = createApp(drawingPreviewFullscreenDialog)

  function createDrawing() {
    const bfContainerDom = document.querySelector('.bf-container')
    const _dom = document.createElement('div')
    _dom.id = 'bfDrawingView'
    bfContainerDom.appendChild(_dom)

    drawingApp.mount('#bfDrawingView', {
      modelValue: false,
      serialNumber: undefined,
      productId: undefined,
      productType: undefined
    })
    drawingApp._instance.emitsOptions = {
      'update:modelValue': (val) => { drawingApp._instance.props.modelValue = val }
    }
    console.log(drawingApp, 'drawingApp')
  }

  function fetchDrawing({ boolBim, serialNumber, productId, productType }) {
    drawingApp._instance.props.modelValue = true
    drawingApp._instance.props.boolBim = boolBim
    drawingApp._instance.props.serialNumber = serialNumber
    drawingApp._instance.props.productId = productId
    drawingApp._instance.props.productType = productType
  }

  return {
    createDrawing,
    fetchDrawing
  }
}

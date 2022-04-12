import { createApp, ref } from 'vue'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

export default function useDrawing() {
  const drawingApp = ref()

  function createDrawing() {
    drawingApp.value = createApp(drawingPreviewFullscreenDialog)
    const bfContainerDom = document.querySelector('.bf-container')
    const _dom = document.createElement('div')
    _dom.id = 'bfDrawingView'
    bfContainerDom.appendChild(_dom)

    drawingApp.value.mount('#bfDrawingView', {
      modelValue: false,
      serialNumber: undefined,
      productId: undefined,
      productType: undefined
    })
    console.log(drawingApp, 'drawingApp')
    drawingApp.value._container._vnode.component.emitsOptions = {
      'update:modelValue': (val) => { drawingApp.value._container._vnode.component.props.modelValue = val }
    }
  }

  function fetchDrawing({ boolBim, serialNumber, productId, productType }) {
    drawingApp.value._container._vnode.component.props.modelValue = true
    drawingApp.value._container._vnode.component.props.boolBim = boolBim
    drawingApp.value._container._vnode.component.props.serialNumber = serialNumber
    drawingApp.value._container._vnode.component.props.productId = productId
    drawingApp.value._container._vnode.component.props.productType = productType
  }

  return {
    createDrawing,
    fetchDrawing
  }
}

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
    console.log(drawingApp, 'drawingApp')
    drawingApp._container._vnode.component.emitsOptions = {
      'update:modelValue': (val) => { drawingApp._container._vnode.component.props.modelValue = val }
    }
  }

  function fetchDrawing({ boolBim, serialNumber, productId, productType }) {
    drawingApp._container._vnode.component.props.modelValue = true
    drawingApp._container._vnode.component.props.boolBim = boolBim
    drawingApp._container._vnode.component.props.serialNumber = serialNumber
    drawingApp._container._vnode.component.props.productId = productId
    drawingApp._container._vnode.component.props.productType = productType
  }

  return {
    createDrawing,
    fetchDrawing
  }
}

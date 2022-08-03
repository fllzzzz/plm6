import { createApp, ref } from 'vue'
import bfDraw from '@/components-system/bim/bf-draw.vue'

export default function useDrawing() {
  const drawingApp = ref()

  function createDrawing() {
    drawingApp.value = createApp(bfDraw)
    const bfContainerDom = document.querySelector('.bf-container')
    const _dom = document.createElement('div')
    _dom.id = 'bfDrawingView'
    bfContainerDom.appendChild(_dom)
    drawingApp.value.mount('#bfDrawingView', {
      serialNumber: undefined,
      productId: undefined,
      productType: undefined
    })
    console.log(drawingApp, 'drawingApp')
  }

  function handleClose() {
    drawingApp.value._container._vnode.component.props.showDraw = false
  }

  function fetchDrawing({ boolBim, serialNumber, productId, productType, drawingSN }) {
    drawingApp.value._container._vnode.component.props.handleClose = handleClose
    drawingApp.value._container._vnode.component.props.showDraw = true
    drawingApp.value._container._vnode.component.props.boolBim = boolBim
    drawingApp.value._container._vnode.component.props.serialNumber = serialNumber
    drawingApp.value._container._vnode.component.props.productId = productId
    drawingApp.value._container._vnode.component.props.productType = productType
    drawingApp.value._container._vnode.component.props.drawingSN = drawingSN
  }

  return {
    createDrawing,
    fetchDrawing
  }
}

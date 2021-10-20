// TODO:未完成
import { reactive, ref, toRefs, onBeforeMount, onMounted, onUnmounted } from 'vue'

export const useTableMaxHeight = ({ paginate = true, isAppContainer = true, extra = 0, hasFixedHeader = true, hPct = 100 }) => {
  let headHeight = 0
  let fixedHeight = 0
  const paginateHeight = paginate ? 40 : 0
  const containerPadding = isAppContainer ? 40 : 0
  if (head) {
    this.head = head
    headHeight = this.currentHeader.height
    // head为字符串类型，调用获取高度
    if (typeof head === 'string') {
      headHeight = this.getHeadHeight(head)
    }
    // head为对象类型,直接从对象中获取高度
    if (typeof head === 'object') {
      headHeight = head.offsetHeight || 0
    }
  }
  const app = document.getElementById('app')
  if (!app || !dom || app.contains(dom)) { // 判断是否在app的盒子中
    const fixedHeader = document.getElementsByClassName('fixed-header')
    if (hasFixedHeader && fixedHeader && fixedHeader.length > 0) {
      const fixedHeadRect = fixedHeader[0].getBoundingClientRect() || {}
      fixedHeight = fixedHeadRect.height || 0
    }
  }

  const clientHeight = this.clientRect.clientHeight || document.documentElement.clientHeight
  if (clientHeight) {
    const height = clientHeight * (hPct / 100) - fixedHeight - containerPadding - paginateHeight - headHeight - extra
    // console.log(clientHeight, fixedHeight, containerPadding, paginateHeight, headHeight, extra)
    return height && height > 0 ? height : 0
  } else {
    return 0
  }
}

export const useTableHeightInit = ({ head = '.head-container' } = {}) => {
  const headHeight = ref(0)
  const clientRect = reactive({ clientHeight: 0, clientWidth: 0 })
  onBeforeMount(() => {
    window.addEventListener('resize', windowSizeHandler, { passive: false })
  })

  onMounted(() => {
    windowSizeHandler()
  })

  onUnmounted(() => {
    window.removeEventListener('resize', windowSizeHandler)
  })

  const windowSizeHandler = () => {
    const rect = document.documentElement
    clientRect.clientHeight = rect.clientHeight || 0
    clientRect.clientWidth = rect.clientWidth || 0

    if (head) {
      const height = getHeadHeight(head)
      headHeight.value = height || 0
    }
  }

  return {
    headHeight: toRefs(headHeight),
    clientRect: toRefs(clientRect)
  }
}

function getHeadHeight(head) {
  let headHeight = 0
  if (typeof head === 'string') {
    const heads = head.split(',')
    heads.forEach((h) => {
      let type
      let rect
      h = h.trim()
      if (h.substr(0, 1) === '.') {
        type = 'class'
      }
      if (h.substr(0, 1) === '#') {
        type = 'id'
      }
      if (!type) return
      h = h.substr(1, h.length)
      let headDom
      if (type === 'class') {
        headDom = document.getElementsByClassName(h)
        rect = headDom && headDom.length > 0 ? headDom[0].getBoundingClientRect() : {}
        headHeight += rect.height || 0
      }
      if (type === 'id') {
        rect = document.getElementById(h)
        headHeight += rect ? rect.offsetHeight : 0
      }
    })
  }
  return headHeight
}

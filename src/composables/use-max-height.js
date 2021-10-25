import { onBeforeMount, onMounted, onUnmounted, ref, nextTick } from 'vue'
import { cleanArray } from '@/utils/data-type/array'
import { getStyle, style2Num } from '@/utils/element/style'

// 分页插件
const EL_PAGINATION = '.el-pagination'
const NAVBAR = '#navbar'
/**
 * 为了保证页面内部不出现滚动条计算dom的最大高度
 * @param {string | Array} extraDom 默认：.head-container。需要删去高度的dom 可传入id或class
 * @param {boolean} paginate 是否存在分页插件。
 * @param {boolean} navbar 是否存在navbar。Layout-navbar
 * @param {boolean} appContainer 是被被app-container包裹
 * @param {number} extraHeight 需要减去的额外高度
 * @returns
 */
export default function test({ extraDom = '.head-container', wrapperDom = '.app-container', navbar = true, paginate = false, extraHeight = 0 } = {}) {
  const maxHeight = ref(0)

  // 监听resize
  onBeforeMount(() => {
    window.addEventListener('resize', windowSizeHandler, { passive: false })
  })

  onMounted(() => {
    // TODO:首次触发wrapper层的padding未获取成功,具体是否使用,看视图是否会有明显变化
    // windowSizeHandler()
    nextTick(() => {
      windowSizeHandler()
    })
  })

  // 取消resize
  onUnmounted(() => {
    window.removeEventListener('resize', windowSizeHandler)
  })

  const windowSizeHandler = () => {
    maxHeight.value = calcMaxHeight({ extraDom, wrapperDom, navbar, paginate, extraHeight })
  }

  return maxHeight
}

// 计算最大高度
function calcMaxHeight({ extraDom, navbar, wrapperDom, paginate, extraHeight = 0 }) {
  const rect = document.documentElement
  // 窗口高度及宽度
  const clientHeight = rect.clientHeight || 0

  // 所传入dom组件的高度
  const extraDomHeight = getDomHeight(extraDom)

  // 包裹在外层的dom的边距之和
  const wrapperDomHeight = getWrapperDomHeight(wrapperDom)

  // 分页组件高度(不考虑页面多个分页组件的情况)
  const paginateHeight = paginate ? getDomHeight(EL_PAGINATION) : 0

  // navbar高度
  const navbarHeight = navbar ? getDomHeight(NAVBAR) : 0

  // 窗口高度 - navbar高度 - 包装层内外边距 - 额外dom的高度（含外边距） - 分页插件的高度 - 自定义额外高度
  // 注意：未处理外边距重叠的情况，若产生，可通过填写extraHeight处理
  const height = clientHeight - navbarHeight - wrapperDomHeight - extraDomHeight - paginateHeight - extraHeight

  return height > 0 ? height : 0
}

// 获取包装的高度（边距）
function getWrapperDomHeight(wrapperDom) {
  const styleProps = ['paddingTop', 'paddingBottom', 'marginTop', 'marginBottom']
  return calcHeight(wrapperDom, styleProps)
}

// 获取dom的高度
function getDomHeight(dom) {
  const styleProps = ['height', 'marginTop', 'marginBottom']
  return calcHeight(dom, styleProps)
}

// 计算高度
function calcHeight(doms, styleProps) {
  let domHeight = 0
  let _doms = doms
  if (!(_doms instanceof Array)) {
    _doms = _doms ? [_doms] : []
  }
  // 转换dom格式
  _doms = domFormatter(_doms)
  _doms.forEach((h) => {
    let el
    if (h.type === 'class') {
      el = document.getElementsByClassName(h.name)
      el = el ? el[0] : null
    }
    if (h.type === 'id') {
      el = document.getElementById(h.name)
    }
    if (el) {
      domHeight = styleProps.reduce((sum, curStyle) => {
        return sum + style2Num(getStyle(el, curStyle)) || 0
      }, domHeight)
    }
  })
  return domHeight
}

// dom格式转换 '.app' => { name:'app', type:'class' }
function domFormatter(dom) {
  let doms = []

  if (dom instanceof Array) {
    doms = JSON.parse(JSON.stringify(dom))
  }

  if (typeof dom === 'string') {
    doms = dom.split(',')
  }

  const _dom = doms.map(name => {
    let type
    name = name.trim()
    if (name.substr(0, 1) === '.') {
      type = 'class'
    }
    if (name.substr(0, 1) === '#') {
      type = 'id'
    }
    if (type) {
      return {
        name: name.substr(1, name.length),
        type
      }
    } else {
      return null
    }
  })

  return cleanArray(_dom)
}


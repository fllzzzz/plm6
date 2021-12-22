import { onMounted, onBeforeUnmount, ref, nextTick, watch, onUpdated } from 'vue'
import { cleanArray } from '@data-type/array'
import { elHScrollable } from '@/utils/element/index'
import { getStyle, style2Num, splitStyleOfNum } from '@/utils/element/style'
import { isBlank, isNotBlank } from '@/utils/data-type'

// 分页插件
const EL_PAGINATION = '.el-pagination'
const NAVBAR = '#navbar'
/**
 * TODO: 考虑有很多模块extraBox与wrapperBox是重复的，设置为指定模式，不用填写
 * TODO: 考虑可以直接传入el
 * 为了保证页面内部不出现滚动条计算dom的最大高度
 * @param {string | Array} main 尽量传入主盒子，会检测其他盒子是否处于主盒子，可以有效的避免多个class无法取得正确的class的问题。（极端情况仍会有问题）
 * @param {string | Array} extraBox ='.head-container' 需要删去高度的dom 可传入id或class
 * @param {string | Array} wrapperBox = '.app-container' 包裹层
 * @param {boolean} navbar = true 是否存在navbar。Layout-navbar
 * @param {boolean} paginate = false 是否存在分页插件。
 * @param {number | string} extraHeight = 0 需要减去的额外高度 允许px，vh，vw, 其他单位视为px。不带单位视为px
 * @param {number | string} minHeight = 400 最小高度 允许px，vh, vw, 其他单位视为px。不带单位视为px
 * @param {boolean} clientHRepMainH = false 使用窗口高度代替主盒子高度。避免类似于dialog这样“高度由子盒子撑开的”dom。
 * @param {computed(boolean), Function} trigger 开始监听.function 的返回值需要是可监听的对象
 * @returns
 */
export default function useMaxHeight(
  {
    mainBox,
    extraBox = '.head-container',
    wrapperBox = '.app-container',
    navbar = !mainBox,
    paginate = false,
    extraHeight = 0,
    minHeight = 400,
    clientHRepMainH = false
  } = {},
  trigger
) {
  const maxHeight = ref(0)
  const maxHeightStyle = ref()
  const heightStyle = ref()
  const isBind = ref(false)

  onMounted(() => {
    if (isNotBlank(trigger)) {
      let wv
      switch (trigger.constructor.name) {
        case 'Function':
        case 'RefImpl':
        case 'ComputedRefImpl':wv = trigger
          break
        default: wv = trigger
      }
      watch(
        wv,
        (flag) => {
          if (flag) {
            bindEventListener(windowSizeHandler, isBind)
          } else {
            unbindEventListener(windowSizeHandler)
          }
        },
        { immediate: true }
      )
    } else {
      bindEventListener(windowSizeHandler, isBind)
    }
  })

  onUpdated(() => {
    // 避免数据渲染后出现滚动条未处理
    if (isBind.value) {
      windowSizeHandler()
    }
  })

  // 取消resize
  onBeforeUnmount(() => {
    unbindEventListener(windowSizeHandler)
  })

  const windowSizeHandler = () => {
    maxHeight.value = calcMaxHeight({ clientHRepMainH, mainBox, extraBox, wrapperBox, navbar, paginate, extraHeight, minHeight })
    heightStyle.value = `height: ${maxHeight.value}px`
    maxHeightStyle.value = `max-height: ${maxHeight.value}px`
  }

  return {
    maxHeight,
    heightStyle,
    maxHeightStyle
  }
}

// 计算最大高度
function calcMaxHeight({ clientHRepMainH, mainBox, extraBox, wrapperBox, navbar, paginate, extraHeight, minHeight }) {
  // 主盒子高度
  const [mainBoxEl, mainBoxHeight] = getMainBoxHeight(mainBox)

  // 所传入dom组件的高度
  const extraBoxHeight = getDomHeight(extraBox, mainBoxEl)

  // 包裹在外层的dom的边距之和
  const wrapperBoxHeight = getWrapperBoxHeight(wrapperBox, mainBoxEl)

  // 分页组件高度(不考虑页面多个分页组件的情况)
  const paginateHeight = paginate ? getDomHeight(EL_PAGINATION, mainBoxEl) : 0

  // navbar高度
  const navbarHeight = navbar ? getDomHeight(NAVBAR, mainBoxEl) : 0

  // 实际额外高度
  const realExtraHeight = getRealHeight(extraHeight)

  // 实际最小高度
  const realMiniHeight = getRealHeight(minHeight)

  // 滚动条高度(出现横向滚动条时，要减少滚动条高度)
  const horizontalScrollBarHeight = getHorizontalScrollBarHeight(mainBoxEl)

  // 窗口高度 - navbar高度 - 包装层内外边距 - 额外dom的高度（含外边距） - 分页插件的高度 - 自定义额外高度
  // 注意：未处理外边距重叠的情况，若产生，可通过填写extraHeight处理
  let mainHeight
  if (clientHRepMainH) {
    // 如果使用client作为高度，则需要减去主盒子的外边距和内边距
    mainHeight = document.documentElement.clientHeight - getElHeight(mainBoxEl, ['marginTop', 'marginBottom', 'paddingTop', 'paddingBottom'])
  } else {
    mainHeight = mainBoxHeight
  }
  // -1 避免特殊情况下高度正好卡在滚动条出与不出之间，因此-1 避免这种情况（理论浏览器全屏，显示大小未125%状态下）
  const height = mainHeight - navbarHeight - wrapperBoxHeight - extraBoxHeight - paginateHeight - realExtraHeight - horizontalScrollBarHeight - 1
  // console.log(extraBox, mainBoxHeight, mainHeight, navbarHeight, wrapperBoxHeight, extraBoxHeight, paginateHeight, realExtraHeight, horizontalScrollBarHeight)

  return height > realMiniHeight ? height : realMiniHeight
}

// 获取包装的高度（边距）
function getWrapperBoxHeight(wrapperBox, mainBoxEl) {
  const styleProps = ['paddingTop', 'paddingBottom']
  return calcHeight(wrapperBox, mainBoxEl, styleProps)
}

// 获取dom的高度
function getDomHeight(dom, mainBoxEl) {
  const styleProps = ['height', 'marginTop', 'marginBottom']
  return calcHeight(dom, mainBoxEl, styleProps)
}

// 计算高度
function calcHeight(doms, mainBoxEl, styleProps) {
  let domHeight = 0
  let _doms = doms
  if (!(_doms instanceof Array)) {
    _doms = _doms ? [_doms] : []
  }
  // 转换dom格式
  _doms = domFormatter(_doms)
  _doms.forEach((h) => {
    const el = getElement(h, mainBoxEl)
    if (el) {
      domHeight += getElHeight(el, styleProps)
    }
  })
  return domHeight
}

// 计算实际高度
function getElHeight(el, styleProps) {
  return styleProps.reduce((sum, curStyle) => {
    return sum + style2Num(getStyle(el, curStyle)) || 0
  }, 0)
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

  const _dom = doms.map((name) => {
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

/**
 * 获取el
 * @param {*} dom domInfo { type:'class', name:'box'}
 * @param {*} ancestorDomEl 祖先节点
 * @returns
 */
function getElement(dom, ancestorDomEl) {
  if (dom.type === 'class') {
    const elArr = document.getElementsByClassName(dom.name)
    if (isBlank(elArr)) return null
    if (ancestorDomEl) {
      // 判断是否被祖先包含
      for (let i = 0; i < elArr.length; i++) {
        const curEl = elArr[i]
        if (ancestorDomEl.contains(curEl)) {
          return curEl
        }
      }
    } else {
      return elArr[0]
    }
  }
  if (dom.type === 'id') {
    const curEl = document.getElementById(dom.name)
    if (ancestorDomEl) {
      // 判断是否被祖先包含
      if (ancestorDomEl.contains(curEl)) {
        return curEl
      }
    } else {
      return curEl
    }
  }
  // console.log('未找到正确的盒子', dom, ancestorDomEl)
  // throw new Error('未找到正确的盒子')
  return null
}

// 获取主盒子高度
function getMainBoxHeight(mainBox) {
  if (isBlank(mainBox)) {
    // 主盒子不存在，则视主盒子为浏览器窗口
    return [document.documentElement, document.documentElement.clientHeight || 0]
  }
  // dom-name => dom-info
  const box = domFormatter(mainBox)
  let lastBox
  if (box.length > 0) {
    // 多层级的
    let prevBox = null
    box.forEach((b, index) => {
      const curBox = getElement(b, prevBox)
      prevBox = curBox
    })
    lastBox = prevBox
  } else {
    lastBox = getElement(box)
  }

  // 高度 - 内边距 为实际可用高度
  const elHeight = getElHeight(lastBox, ['height'])
  const elPadding = getElHeight(lastBox, ['paddingTop', 'paddingBottom'])
  return [lastBox, elHeight - elPadding]
}

// 获取横向滚动条高度
function getHorizontalScrollBarHeight(mainBoxEl) {
  const able = elHScrollable(mainBoxEl)
  // 移动时有时仍会出现滚动条 与 盒子不为浏览器窗口时，未处理
  const scrollWidth = window.innerWidth - document.body.clientWidth //  = 10
  if (able) {
    return scrollWidth
  }
  return 0
}

// 计算高度
function getRealHeight(heightStyle) {
  if (isBlank(heightStyle)) {
    return 0
  }
  const res = splitStyleOfNum(heightStyle)
  let height
  switch (res[1]) {
    case 'vh':
      height = (document.documentElement.clientHeight * res[0]) / 100
      break
    case 'vw':
      height = (document.documentElement.clientWidth * res[0]) / 100
      break
    default:
      height = res[0]
  }
  return height
}

// 绑定监听事件
function bindEventListener(fn, isBind) {
  nextTick(() => {
    fn() // 第一次手动触发
    window.addEventListener('resize', fn, { passive: false })
    isBind.value = true
  })
}

// 移除监听事件
function unbindEventListener(fn) {
  window.removeEventListener('resize', fn)
}


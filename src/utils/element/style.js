import { isBlank } from '@data-type/index'

// 获取el样式
export function getStyle(el, styleProp) {
  // console.log('window.getComputedStyle(el)', window.getComputedStyle(el))
  return window.getComputedStyle(el)[styleProp]
}

// TODO: 弃用，使用window.getComputedStyle
export function getStyle2(el, styleProp) {
  let value
  const defaultView = (el.ownerDocument || document).defaultView
  // W3C standard way:
  if (defaultView && defaultView.getComputedStyle) {
    // sanitize property name to css notation
    // (hypen separated words eg. font-Size)
    styleProp = styleProp.replace(/([A-Z])/g, '-$1').toLowerCase()
    return defaultView.getComputedStyle(el, null).getPropertyValue(styleProp)
  } else if (el.currentStyle) { // IE
    // sanitize property name to camelCase
    styleProp = styleProp.replace(/\-(\w)/g, function (str, letter) {
      return letter.toUpperCase()
    })
    value = el.currentStyle[styleProp]
    // convert other units to pixels on IE
    if (/^\d+(em|pt|%|ex)?$/i.test(value)) {
      return (function (value) {
        var oldLeft = el.style.left
        var oldRsLeft = el.runtimeStyle.left
        el.runtimeStyle.left = el.currentStyle.left
        el.style.left = value || 0
        value = el.style.pixelLeft + 'px'
        el.style.left = oldLeft
        el.runtimeStyle.left = oldRsLeft
        return value
      })(value)
    }
    return value
  }
}

// 20px -> 20
export function style2Num(str) {
  if (isBlank(str)) return
  const res = str.match(/^\d+\.?\d*/)
  if (isBlank(res)) return // TODO: auto 未处理
  const number = Number(res[0])
  return number
}

// 20px -> [20, 'px']
export function splitStyleOfNum(val) {
  if (isBlank(val)) return [null, null]
  if (typeof val === 'string') {
    const res = val.match(/^\d+\.?\d*/)
    if (isBlank(res)) return [null, null]
    const numStr = res[0]
    return [
      Number(numStr),
      val.substring(numStr.length)
    ]
  }
  if (typeof val === 'number') {
    return [val, null]
  }
}

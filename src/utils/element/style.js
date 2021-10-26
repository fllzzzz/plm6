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
  const number = Number(str.match(/^\d+\.?\d*/)[0])
  return number
}

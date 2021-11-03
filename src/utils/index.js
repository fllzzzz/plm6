/**
 * 工具类
 */
import { validStartSymbol } from '@/utils/validate'
import { cleanArray } from '@data-type/array'

/**
 * 获取url中查询内容，并合并为对象
 * @param {string} url
 * @returns {Object}
 */
export function getQueryObject(url) {
  url = url == null ? window.location.href : url
  const search = url.substring(url.lastIndexOf('?') + 1)
  const obj = {}
  const reg = /([^?&=]+)=([^?&=]*)/g
  search.replace(reg, (rs, $1, $2) => {
    const name = decodeURIComponent($1)
    let val = decodeURIComponent($2)
    val = String(val)
    obj[name] = val
    return rs
  })
  return obj
}

/**
 * 拼接url
 * @param {string} url
 * @param {string} data
 */
export function spliceUrl(url, data) {
  if (!url) {
    return
  }
  var params = []
  Object.keys(data).forEach(key => {
    let value = data[key]
    // 如果值为undefined,将其置空
    if (typeof value === 'undefined') {
      value = ''
    }
    // 对于需要编码的文本（比如说中文）要进行编码
    params.push([key, encodeURIComponent(value)].join('='))
    // params.push([key, value].join('='))
  })
  return `${url}?${params.join('&')}`
}

/**
 * 将json转为url路径参数并拼接
 * @param {Object} json
 * @returns {Array}
 */
export function param(json) {
  if (!json) return ''
  return cleanArray(
    Object.keys(json).map(key => {
      if (json[key] === undefined) return ''
      return encodeURIComponent(key) + '=' + encodeURIComponent(json[key])
    })
  ).join('&')
}

/**
 * url中的参数转对象
 * @param {string} url
 * @returns {Object}
 */
export function param2Obj(url) {
  const search = url.split('?')[1]
  if (!search) {
    return {}
  }
  return JSON.parse(
    '{"' +
      decodeURIComponent(search)
        .replace(/"/g, '\\"')
        .replace(/&/g, '","')
        .replace(/=/g, '":"')
        .replace(/\+/g, ' ') +
      '"}'
  )
}

/**
 * html2Text
 * @param {string} val
 * @returns {string}
 */
export function html2Text(val) {
  const div = document.createElement('div')
  div.innerHTML = val
  return div.textContent || div.innerText
}

// 替换邮箱字符
export function regEmail(email) {
  if (String(email).indexOf('@') > 0) {
    const str = email.split('@')
    let _s = ''
    if (str[0].length > 3) {
      for (var i = 0; i < str[0].length - 3; i++) {
        _s += '*'
      }
    }
    var new_email = str[0].substr(0, 3) + _s + '@' + str[1]
  }
  return new_email
}

// 替换手机字符
export function regMobile(mobile) {
  if (mobile.length > 7) {
    var new_mobile = mobile.substr(0, 3) + '****' + mobile.substr(7)
  }
  return new_mobile
}

/**
 * 模10算法(可用于银行卡)
 * @param {string|number} num
 */
export function luhn(num) {
  num = (num + '')
    .replace(/\D+/g, '')
    .split('')
    .reverse()
  if (!num.length) {
    return false
  }
  var total = 0
  var i
  for (i = 0; i < num.length; i++) {
    num[i] = parseInt(num[i])
    total += i % 2 ? 2 * num[i] - (num[i] > 4 ? 9 : 0) : num[i]
  }
  if (total === 0) {
    return false
  }
  return total % 10 === 0
}

// 等待
export function codeWait(time = 500) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve()
    }, time)
  })
}

/**
 *
 * 设置起始符号
 * @export
 * @param {*} routePath
 * @param {*} symbol
 * @returns
 */
export function repairStartSymbol(routePath, symbol) {
  if (!routePath) {
    return
  }
  // console.log('routePath', routePath, validStartSymbol(routePath, symbol))
  if (validStartSymbol(routePath, symbol)) {
    return routePath
  } else {
    return `${symbol}${routePath}`
  }
}

// 随机浅色颜色
export function getColorUndertint() {
  return '#' +
         (function random(color) {
           return (color += '5678956789defdef'[Math.floor(Math.random() * 16)]) &&
             (color.length === 6) ? color : random(color)
         })('')
}

/**
 * @desc  函数防抖
 * @param  func 需要执行的函数
 * @param  wait 延迟执行时间（毫秒）
 * @param  immediate---true 表立即执行，false 表非立即执行
 **/
export function debounce(func, wait, immediate) {
  let timer

  return function () {
    const context = this
    const args = arguments

    if (timer) clearTimeout(timer)
    if (immediate) {
      var callNow = !timer
      timer = setTimeout(() => {
        timer = null
      }, wait)
      if (callNow) func.apply(context, args)
    } else {
      timer = setTimeout(function () {
        func.apply(context, args)
      }, wait)
    }
  }
}

/**
 * @desc 函数节流
 * @param func 函数
 * @param wait 延迟执行毫秒数
 * @param type 1 表时间戳版，2 表定时器版
 */
export function throttle(func, wait, type = 1) {
  let previous = 0
  let timeout

  return function () {
    const context = this
    const args = arguments
    if (type === 1) {
      const now = Date.now()

      if (now - previous > wait) {
        func.apply(context, args)
        previous = now
      }
    } else if (type === 2) {
      if (!timeout) {
        timeout = setTimeout(() => {
          timeout = null
          func.apply(context, args)
        }, wait)
      }
    }
  }
}

/**
 * 判断对象中的字段是否发生改变，适用于对象中存储了oldVal与newVal的情况，如 sourceHeight 与 height
 * @param {object} item 校验对象
 * @param {map} fieldMap 需要校验的字段，oldVal与newVal的字段名 以键值对的方式存储
 * @returns 是否变化。true:发生变化
 */
export function judgeItemFieldChange(item, fieldMap) {
  for (const [key, value] of fieldMap) {
    if (item[key] !== item[value]) return true
  }
  return false
}

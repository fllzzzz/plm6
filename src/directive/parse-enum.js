import { emptyTextFormatter, isBlank } from '@/utils/data-type'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'

// 枚举转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  },
  updated(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  // e:enum 枚举, v: 枚举值
  const {
    value: { e, v, f = 'L', bit = false, split = '、', emptyText, extra = '' }
  } = binding
  // 先设置为空，避免v为null或找不到的状态下，innerText是之前的值
  el.innerText = ''
  if (isBlank(v)) return

  let _enumV = e.V
  if (isBlank(_enumV)) {
  // 处理某些页面自定义而没有经过处理的枚举
    _enumV = {}
    const KEYS = Object.keys(e)
    KEYS.forEach((key) => {
      const value = e[key].V
      _enumV[value] = e[key]
    })
  }
  if (isBlank(_enumV)) return
  let text
  if (Array.isArray(v)) {
    text = v.map((v) => _enumV[v][f]).join(split)
  } else if (bit) {
    text = cleanArray(EO.getBits(e, v, f)).join(split)
  } else {
    text = _enumV[v][f] || ''
  }
  if (!text) {
    return
  }
  text += extra
  el.innerText = emptyText ? emptyTextFormatter(text, emptyText) : text
}

import { emptyTextFormatter } from '@/utils/data-type'
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
  const { value: { e, v, bit = false, split = '、', emptyText, extra = '' }} = binding
  let text
  if (Array.isArray(v)) {
    text = v.map(v => e.VL[v]).join(split)
  } else if (bit) {
    text = cleanArray(EO.getBits(e, v, 'L')).join(split)
  } else {
    text = e.VL[v] || ''
  }
  if (!text) return
  text += extra
  el.innerText = emptyText ? emptyTextFormatter(text, emptyText) : text
}

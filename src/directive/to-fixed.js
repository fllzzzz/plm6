import { isNotBlank, toFixed } from '@/utils/data-type'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  },
  updated(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value } = binding
  const { innerText } = el
  const precision = isNotBlank(value) ? value : 2
  el.innerText = toFixed(innerText, precision)
}

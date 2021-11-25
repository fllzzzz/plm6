import { toFixed } from '@/utils/data-type'

// 时间转换
export default {
  updated(el, binding) {
    const { value } = binding
    const { innerText } = el
    el.innerText = toFixed(innerText, value || 2)
  }
}

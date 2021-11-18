import { parseTime } from '@/utils/date'

// 时间转换
export default {
  mounted(el, binding) {
    const { value } = binding
    const { innerText } = el
    el.innerText = parseTime(innerText, value)
  }
}

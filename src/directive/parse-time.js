import { parseTime } from '@/utils/date'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value } = binding
  const { innerText } = el
  el.innerText = parseTime(innerText, value || '{y}-{m}-{d} {h}:{i}')
}

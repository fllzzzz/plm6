import { parseTime } from '@/utils/date'

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
  if (value !== null && typeof value === 'object') {
    const { val, fmt = '{y}-{m}-{d} {h}:{i}' } = value
    el.innerText = parseTime(val, fmt)
  } else {
    el.innerText = parseTime(innerText, value || '{y}-{m}-{d} {h}:{i}')
  }
}

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
  let val
  let precision
  if (value && typeof value === 'object') {
    val = value.val
    precision = value.dp
  } else {
    val = innerText
    precision = value
  }
  precision = isNotBlank(precision) ? precision : 2
  el.innerText = toFixed(val, precision)
}

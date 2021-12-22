import { isNotBlank, toFixed } from '@/utils/data-type'
import { DP } from '@/settings/config'
import { isString } from '@/utils/validate/index'

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
    precision = isString(value.dp) ? DP[value.dp] : value.dp
  } else {
    val = innerText
    precision = isString(value) ? DP[value] : value
  }
  precision = isNotBlank(precision) ? precision : 2
  el.innerText = toFixed(val, precision)
}

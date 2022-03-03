import { isNotBlank, toFixed } from '@/utils/data-type'
import { DP } from '@/settings/config'

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
  let DP_precision
  if (value && typeof value === 'object') {
    val = value.val
    precision = value.dp
    DP_precision = DP[value.k]
  } else {
    val = innerText
    precision = value
  }
  console.log('val, precision', val, precision)
  precision = isNotBlank(precision) ? precision : isNotBlank(DP_precision) ? DP_precision : 2
  el.innerText = toFixed(val, precision)
}

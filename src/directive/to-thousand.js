import { isNotBlank } from '@/utils/data-type'
import { toThousand } from '@/utils/data-type/number'

// 千分位数值
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

// value 小数精度
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
  el.innerText = toThousand(val, precision)
}

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
  const precision = isNotBlank(value) ? value : 2
  el.innerText = toThousand(innerText, precision)
}

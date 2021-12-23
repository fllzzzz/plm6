import { emptyTextFormatter, isBlank } from '@/utils/data-type'

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
  const { value: { val, blank = '-' }} = binding
  let v = val
  if (isBlank(v)) v = undefined
  el.innerText = emptyTextFormatter(v, blank)
}

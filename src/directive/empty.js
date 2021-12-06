import { emptyTextFormatter, isBlank } from '@/utils/data-type'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value: { val, split = '-' }} = binding
  let v
  if (isBlank(val)) v = undefined
  el.innerText = emptyTextFormatter(v, split)
}

import { emptyTextFormatter } from '@/utils/data-type'

// 空字符串 判断，可配合表格row-key使用，否则该数据不会更新
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
    const { val, blank = '-' } = value
    el.innerText = emptyTextFormatter(val, blank)
  } else {
    el.innerText = emptyTextFormatter(value || innerText, '-')
  }
}

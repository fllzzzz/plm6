import { isJSON } from '@/utils/data-type'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value } = binding
  const { innerText } = el
  if (!isJSON(innerText)) return
  const text = JSON.parse(innerText)
  if (Array.isArray(text)) {
    el.innerText = text.join(value || '、')
  }
}

import { toThousand } from '@/utils/data-type/number'

// 千分位数值
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  if (el.tagName.toLocaleUpperCase() !== 'INPUT') {
    el = el.getElementsByTagName('input')[0]
  }
  el.onblur = (e) => {
    el.value = toThousand(el.value)
  }
}

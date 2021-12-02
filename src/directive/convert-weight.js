import { MIN_UNIT } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value: { unit = 'kg', dp = 2 } = {}} = binding
  const { innerText } = el
  el.innerText = convertUnits(innerText, MIN_UNIT.WEIGHT, unit, dp, { toNum: false })
}

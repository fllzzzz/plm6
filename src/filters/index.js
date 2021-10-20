import { parseTime as parseTimeUtils } from '@/utils'

export const parseTime = () => {
  return function (time, cFormat) {
    parseTimeUtils(time, cFormat)
  }
}

const map = new Map([
  ['parseTime', parseTime]
])
export const filterGetter = (methodsGetter) => {
  return methodsGetter.reduce((res, cur) => {
    const fun = map.get('cur')
    if (typeof fun === 'function') {
      res[cur] = fun
    }
  }, {})
}

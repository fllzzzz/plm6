import { pinyin } from 'pinyin-pro'
import { isBlank } from './data-type'

// 拼音格式转换
export function pinyinFM(zn, { toneType = 'none' } = {}) {
  return pinyin(zn, { toneType })
}

// 对象、数组 字段拼音转换
export function pinyinForField(data, fields = []) {
  if (typeof data !== 'object' || isBlank(data)) return
  let list = []
  if (Array.isArray(data)) {
    list = data
  } else {
    list = [data]
  }
  if (list.length > 0) {
    list.forEach((row) => {
      fields.forEach((field) => {
        setPinyinForField(row, field)
      })
    })
  }
  return data
}

// 模糊查询
export function pinyinFuzzyMatching(data, matchingInfo, fields = []) {
  if (typeof data !== 'object' || isBlank(data)) return
  let temp = []
  if (Array.isArray(fields)) {
    temp = fields
  } else {
    temp = [fields]
  }
  for (const field of temp) {
    // TODO: 暂时去空格匹配，后期优化
    const flag =
      getInfoForField(data, field).indexOf(matchingInfo) > -1 ||
      getInfoForField(data, `${field}_pinyin`).replaceAll(' ', '').indexOf(matchingInfo) > -1
    if (flag) return true
  }
  return false
}

// 获取信息
function getInfoForField(row, field) {
  if (!field) return
  const keys = field.split('.')
  if (keys.length === 1) {
    return row[keys[0]]
  } else {
    return keys.reduce((cur, key) => {
      return typeof cur === 'object' ? cur[key] : undefined
    }, row)
  }
}

// 设置拼音
function setPinyinForField(row, field) {
  if (!field) return
  const keys = field.split('.')
  // 数组长度为1，则代表field为row对象中的字段
  const pinyinKey = `${keys[keys.length - 1]}_pinyin`
  let obj = row
  for (let i = 0; i < keys.length - 1; i++) {
    obj = obj[keys[i]]
  }
  const zhCn = getInfoForField(row, field)
  obj[pinyinKey] = pinyinFM(zhCn)
}

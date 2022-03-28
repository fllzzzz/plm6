import { isBlank, toFixed } from './data-type'
import { getDP, toThousand } from '@/utils/data-type/number'

/**
 * 合计通用
 * @param {*} param
 * @param {array} props 需要显示合计的列 ['a','b'] || [['a',1], 'b']  1:小数精度
 * @param {number} precision 小数精度
 * @param {array} toThousandFields 金额数字字段 10000 => 10,000
 * @returns
 */
export function tableSummary(param, { props = [], precision, toThousandFields = [] } = {}) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    const fieldIndex = props.findIndex((f) => {
      if (Array.isArray(f)) {
        return f[0] === column.property
      } else {
        return f === column.property
      }
    })
    if (fieldIndex > -1) {
      const values = data.map((item) => {
        let d = item
        if (item && item.sourceRow) {
          d = item.sourceRow
        }
        return Number(getInfo(d, column.property))
      })
      let dp = precision
      const dpArr = []
      const curField = props[fieldIndex]
      if (Array.isArray(curField) && curField.length === 2) {
        dp = curField[1]
      }
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          if (!isNaN(curr)) {
            dpArr.push(getDP(curr))
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        // 获取最大的小数精度位数
        if (isBlank(dp)) {
          dp = dpArr.getMax()
          dp = dp > 2 ? 2 : dp
        }
        if (toThousandFields.includes(column.property)) {
          sums[index] = toThousand(sums[index], dp)
        } else {
          sums[index] = toFixed(sums[index], dp)
        }
      }
    }
  })
  return sums
}

// 获取字段信息
export function getInfo(row, field) {
  if (isBlank(row)) return
  if (field) {
    const keys = field.split('.')
    return keys.reduce((cur, key) => {
      return typeof cur === 'object' && cur ? cur[key] : undefined
    }, row)
  } else {
    return row
  }
}

// 设置字段信息
export function setInfo(row, field, data) {
  if (isBlank(row)) return
  if (field) {
    const keys = field.split('.')
    if (keys.length === 1) {
      row[keys[0]] = data
    } else {
      const preInfo = keys.reduce((cur, key, index) => {
        if (index === keys.length - 1) {
          return cur
        } else {
          return typeof cur === 'object' && cur ? cur[key] : undefined
        }
      }, row)
      if (typeof preInfo === 'object' && preInfo) {
        preInfo[keys[keys.length - 1]] = data
      }
    }
  } else {
    return row
  }
}

import { toFixed } from './data-type'

/**
 * 合计通用
 * @param {*} param
 * @param {array} props 需要显示合计的列 ['a','b'] || [['a',1], 'b']  1:小数精度
 * @returns
 */
export function tableSummary(param, { props = [], precision = 2 }) {
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
      const values = data.map((item) => Number(item[column.property]))
      let dp = precision
      const curField = props[fieldIndex]
      if (Array.isArray(curField) && curField.length === 2) {
        dp = curField[2]
      }
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = toFixed(sums[index], dp)
      }
    }
  })
  return sums
}

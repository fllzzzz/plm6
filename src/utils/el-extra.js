import { toFixed } from './data-type'

/**
 * 合计通用
 * @param {*} param
 * @param {array} props 需要显示合计的列
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
    if (props.includes(column.property)) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = toFixed(sums[index], precision)
      }
    }
  })
  return sums
}

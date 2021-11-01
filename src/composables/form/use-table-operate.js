import { deepClone } from '@/utils/data-type'
/**
 * table操作
 * @param {*} list 列表
 * @param {*} rowNumbers 初始化数量
 * @param {*} defaultInfo 默认参数
 */
export default function useTableOperate(defaultInfo, rowNumbers = 1, dittos) {
  const init = (list) => {
    list.length = 0
    for (let i = 0; i < rowNumbers; i++) {
      addRow(list)
    }
  }

  const addRow = (list) => {
    const row = deepClone(defaultInfo)
    if (dittos && list.length > 0) {
      dittos.forEach((value, key) => {
        row[key] = value
      })
    }
    list.push(row)
  }

  const removeRow = (list, index) => {
    list.splice(index, 1)
  }

  return { init, addRow, removeRow }
}

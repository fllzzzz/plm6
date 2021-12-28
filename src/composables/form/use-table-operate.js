import { deepClone } from '@/utils/data-type'
import { createUniqueString } from '@/utils/data-type/string'
/**
 * table操作
 * @param {*} list 列表
 * @param {*} rowNumbers 初始化数量
 * @param {*} defaultInfo 默认参数
 */
export default function useTableOperate(defaultInfo = {}, rowNumbers = 1, ditto) {
  // 初始行
  const init = (list) => {
    list.length = 0
    for (let i = 0; i < rowNumbers; i++) {
      addRow(list)
    }
  }

  // 添加行
  const addRow = (list) => {
    const row = deepClone(defaultInfo)
    row.uid = createUniqueString()
    if (ditto && list.length > 0) {
      ditto.forEach((value, key) => {
        row[key] = value
      })
    }
    list.push(row)
  }

  // 删除行
  const removeRow = (list, index) => {
    list.splice(index, 1)
  }

  return { init, addRow, removeRow }
}

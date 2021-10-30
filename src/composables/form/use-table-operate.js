import { deepClone } from '@/utils/data-type'
/**
 * table操作
 * @param {*} list 列表
 * @param {*} rowNumbers 初始化数量
 * @param {*} defaultInfo 默认参数
 */
export default function useTableOperate(defaultInfo, rowNumbers = 1) {
  const init = (list) => {
    list.length = 0
    for (let i = 0; i < rowNumbers; i++) {
      list.push(deepClone(defaultInfo))
    }
  }

  const addRow = (list) => {
    list.push(deepClone(defaultInfo))
  }

  const removeRow = (list, index) => {
    list.splice(index, 1)
  }

  return { init, addRow, removeRow }
}

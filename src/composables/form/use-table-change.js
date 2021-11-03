/**
 * 表格信息发生变化， 用于 table的 cell-class-name
 * @param {map} fieldMap 字段map oldVal与newVal的字段名 以键值对的方式存储
 * @returns
 */
export default function useTableChange({ fieldMap }) {
  return {
    changedCellMask: (tableInfo) => changedCellMask(tableInfo, fieldMap)
  }
}

export function changedCellMask({ row, column }, fieldMap) {
  const sourceKey = fieldMap.get(column.property)
  let hasChanged = false
  if (sourceKey) {
    hasChanged = row[column.property] !== row[sourceKey]
  }
  return hasChanged ? 'mask-td' : ''
}

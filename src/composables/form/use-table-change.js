import { isNotBlank } from '@/utils/data-type'

/**
 * 表格信息发生变化， 用于 table的 cell-class-name
 * @param {map} fieldMap 字段map oldVal与newVal的字段名 以键值对的方式存储
 * @param {map} columnsPropsChartMap 字段map 列字段对照表，适用于“一个column.property对应多个字段”或“字段名与column.property不相同”的情况 [column.property, ['校验字段A','校验字段B']]
 * @returns
 */
export default function useTableChange({ fieldMap, columnsPropsChartMap }) {
  let fn
  if (isNotBlank(columnsPropsChartMap)) {
    // 对照表，适用于“一个column.property对应多个字段”或“字段名与column.property不相同”的情况，如备料中的备料范围配置
    fn = (tableInfo) => changedCellMaskOfChart(tableInfo, fieldMap, columnsPropsChartMap)
  } else {
    fn = (tableInfo) => changedCellMask(tableInfo, fieldMap)
  }
  return {
    changedCellMask: fn
  }
}

export function changedCellMask({ row, column }, fieldMap) {
  let hasChanged = false
  const sourceKey = fieldMap.get(column.property)
  if (sourceKey) {
    hasChanged = row[column.property] !== row[sourceKey]
  }
  return hasChanged ? 'mask-td' : ''
}

export function changedCellMaskOfChart({ row, column }, fieldMap, columnsPropsChartMap) {
  let hasChanged = false
  let keys = columnsPropsChartMap.get(column.property)
  if (!Array.isArray(keys)) {
    keys = [keys]
  }
  for (const field of keys) {
    const sourceKey = fieldMap.get(field)
    if (sourceKey) {
      hasChanged = row[field] !== row[sourceKey]
    }
    if (hasChanged) break
  }
  return hasChanged ? 'mask-td' : ''
}

// 默认数据结构
const defaultDS = { key: 'id', label: 'name', value: 'id' }
const dictDS = { key: 'id', label: 'label', value: 'value' }
const enumDS = { key: 'K', label: 'L', value: 'V' }
const enumSLDS = { key: 'K', label: 'SL', value: 'V' }

/**
 * 获取数据结构（一般用于radio及select组件中）
 * @param {string} type  类型：dict/enum/other
 * @param {object} dataStructure 自定义数据结构
 * @returns
 */
export default function useCommonDataStructureByType(type, dataStructure) {
  let DS
  switch (type) {
    case 'dict':
      DS = dictDS
      break
    case 'enum':
      DS = enumDS
      break
    case 'enumSL':
      DS = enumSLDS
      break
    default:
      DS = dataStructure || defaultDS
  }
  return DS
}

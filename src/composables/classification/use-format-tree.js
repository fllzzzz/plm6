import EO from '@enum'
import { classificationEnum } from '@enum-ms/classification'
import { isNotBlank } from '@data-type/index'

const classificationEnumV = EO.key2val(classificationEnum)

// 格式化分类树
export default function useFormatTree(tree, parent, deep = 1) {
  const res = format(tree, parent, deep = 1)
  return res
}

function format(tree, parent, deep = 1) {
  return tree.map(node => {
    const n = {
      parent: parent,
      id: node.id,
      name: node.name,
      code: node.code,
      basicClass: deep === 1 ? node.basicClass : parent.basicClass,
      basicClassName: deep === 1 ? classificationEnumV[`${node.basicClass}`].L : parent.basicClassName,
      serialNumber: `${isNotBlank(parent) ? parent.code : ''}${node.code}`
    }

    if (isNotBlank(node.children)) {
      n.children = format(node.children, n, deep + 1)
      n.isLeaf = false
    } else {
      n.isLeaf = true
    }
    return n
  })
}

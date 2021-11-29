
import { classificationEnum } from '@enum-ms/classification'
import { isNotBlank } from '../data-type'

// 格式化分类树
export function formatClsTree(tree, parent, deep = 1) {
  const res = treeFormat(tree, parent, deep = 1)
  return res
}

function treeFormat(tree, parent, deep = 1) {
  return tree.map(node => {
    const n = {
      parent: parent,
      id: node.id,
      name: node.name,
      code: node.code,
      serialNumber: `${isNotBlank(parent) ? parent.code : ''}${node.code}`
    }
    if (deep === 1) { // 是否为根节点
      n.fullNamePath = [node.name]
      n.basicClass = node.basicClass
      n.basicClassName = classificationEnum.VL[node.basicClass]
    } else {
      n.fullNamePath = [...parent.fullNamePath, node.name]
      n.basicClass = parent.basicClass
      n.basicClassName = parent.basicClassName
    }

    if (isNotBlank(node.children)) {
      n.children = treeFormat(node.children, n, deep + 1)
      n.isLeaf = false
    } else {
      n.isLeaf = true
    }
    return n
  })
}

/**
 * 获取树的第一个叶子节点
 * @param {array} tree 树
 */
export function getFirstLeafNode(tree) {
  const leafNode = getNode(tree)
  return leafNode
}

function getNode(tree) {
  for (let i = 0; i < tree.length; i++) {
    if (tree[i].isLeaf) {
      return tree[i]
    }
    if (isNotBlank(tree[i].children)) {
      getNode(tree)
    }
  }
}

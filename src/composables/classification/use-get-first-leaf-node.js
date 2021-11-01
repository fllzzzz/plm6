
import { isNotBlank } from '@data-type/index'
/**
 * 获取树的第一个叶子节点
 * @param {array} tree 树
 */
export default function useGetFirstLeafNode(tree) {
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

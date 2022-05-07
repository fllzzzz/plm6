/**
 * 树形结构工具类
 */
import { deepClone, isNotBlank } from '@/utils/data-type'

/**
 *
 * 移除节点中数据为空的指定字段
 * @export
 * @param {*} tree
 * @param {string} [emptyFiled='children'] 默认为children字段
 * @returns
 */
export function removeTreeEmptyFiled(tree, emptyFiled = 'children') {
  const treeCopy = JSON.parse(JSON.stringify(tree))

  treeCopy.forEach((node) => {
    if (node[emptyFiled] && node[emptyFiled].length) {
      removeTreeEmptyFiled(node[emptyFiled], emptyFiled)
    } else {
      delete node[emptyFiled]
    }
  })

  return treeCopy
}

/**
 * 树转数组,list-item为树的每个叶子节点
 * @param {*} tree
 */
export function tree2listForLeaf(tree = []) {
  const resArr = []
  tree.forEach((node) => {
    if (isNotBlank(node.children)) {
      const child = tree2listForLeaf(node.children)
      if (isNotBlank(child)) {
        resArr.push.apply(resArr, child)
      }
    } else {
      resArr.push(node)
    }
  })
  return resArr
}

/**
 * 树转数组,list-item为树的每个节点
 * @param {*} tree
 */
export function tree2list(tree = []) {
  const resArr = []
  tree.forEach((node) => {
    resArr.push(node)
    if (isNotBlank(node.children)) {
      const child = tree2list(node.children)
      if (isNotBlank(child)) {
        resArr.push.apply(resArr, child)
      }
    }
  })
  return resArr
}

/**
 * 对树的某一层设置信息
 * @export
 * @param {*} pendingArr 待处理数组
 * @param {*} childFieldName 子节点字段名
 * @param {*} needChangeField 需要修改的字段
 * @param {*} infoField 携带信息的字段
 * @param {*} pollingLimit 轮询次数（树层级）
 * @param {number} [currentIndex=1] 当前轮序下标
 * @author duhh
 */
export function setInfoOfTree(pendingArr, childFieldName, needChangeField, infoField, pollingLimit, currentIndex = 1) {
  if (pendingArr) {
    if (currentIndex === pollingLimit && !!pendingArr) {
      pendingArr.forEach((c) => {
        const infoFieldArr = infoField && infoField.split('.')
        let info = ''
        if (infoFieldArr && c[infoFieldArr[0]]) {
          info = c[infoFieldArr[0]] instanceof Object ? c[infoFieldArr[0]][infoFieldArr[1]] : c[infoFieldArr[0]]
        }
        if (c && info) {
          c[needChangeField] = `${c[needChangeField]}【${info}】`
        }
      })
    }
    if (currentIndex < pollingLimit && !!pendingArr) {
      pendingArr.forEach((c) => {
        setInfoOfTree(c[childFieldName], childFieldName, needChangeField, infoField, pollingLimit, currentIndex + 1)
      })
    }
  }
}

/**
 * 删除树的某一层内的子层级
 * @export
 * @param {*} pendingArr 待处理数组
 * @param {*} childFieldName 子节点字段名
 * @param {*} pollingLimit 轮询次数（树层级）
 * @param {number} [currentIndex=1] 当前轮序下标
 * @author SYJ
 */
export function delNextTree(pendingArr, childFieldName, pollingLimit, currentIndex = 1) {
  if (pendingArr) {
    if (currentIndex === pollingLimit && !!pendingArr) {
      pendingArr.forEach((c) => {
        if (c && c[childFieldName]) {
          c[childFieldName] = null
        }
      })
    }
    if (currentIndex < pollingLimit && !!pendingArr) {
      pendingArr.forEach((c) => {
        delNextTree(c[childFieldName], childFieldName, pollingLimit, currentIndex + 1)
      })
    }
  }
}

/**
 * 根据节点数组获取所需的节点信息
 *
 * @export
 * @param {array} pendingArr 待处理数组
 * @param {array} ids ids 数组
 * @param {string} [idField='id'] id字段名称
 * @param {string} [childField='children'] children字段名称
 * @returns node 节点信息
 * @author duhh
 */
export function getNodeInfoByIds(pendingArr, ids, level = 1, idField = 'id', childField = 'children') {
  const tempFlag = pendingArr && ids && ids.length && level && level <= ids.length && idField && childField
  if (!tempFlag) return
  let arr = JSON.parse(JSON.stringify(pendingArr))
  let nodeInfo
  let currentIndex = 0
  do {
    for (let i = 0; i < arr.length; i++) {
      if (ids[currentIndex] === arr[i][idField]) {
        if (currentIndex === level - 1) {
          nodeInfo = arr[i]
        } else {
          arr = arr[i][childField]
        }
        break
      }
    }
    ++currentIndex
  } while (currentIndex < level)
  return nodeInfo
}

/**
 * 根据节点数组获取所需的节点信息
 *
 * @export
 * @param {array} pendingArr 待处理数组
 * @param {array} ids ids 数组
 * @param {string} [idField='id'] id字段名称
 * @param {string} [childField='children'] children字段名称
 * @returns node 节点信息
 * @author duhh
 */
export function getNodesInfoByIds(pendingArr, ids, level = 1, idField = 'id', childField = 'children') {
  const tempFlag = pendingArr && ids && ids.length && level && level <= ids.length && idField && childField
  if (!tempFlag) return
  const resultArr = []
  let arr = JSON.parse(JSON.stringify(pendingArr))
  let currentIndex = 0
  do {
    for (let i = 0; i < arr.length; i++) {
      if (ids[currentIndex] === arr[i][idField]) {
        const node = JSON.parse(JSON.stringify(arr[i]))
        delete node[childField]
        resultArr.push(node)
        if (currentIndex < level) {
          arr = arr[i][childField]
        }
        break
      }
    }
    ++currentIndex
  } while (currentIndex < level)
  return resultArr
}

/**
 * 根据节点名称数组获取所需的节点信息
 *
 * @export
 * @param {array} pendingArr 待处理数组
 * @param {array} labels 名称数组
 * @param {string} [id='id'] id字段名称
 * @param {string} [label='label'] label字段名称
 * @param {string} [child='children'] children字段名称
 * @returns ids 节点ids
 * @author duhh
 */
export function getNodeIdsByLabels(pendingArr, labels, { id = 'id', label = 'label', child = 'children' } = {}) {
  const level = labels.length
  const tempFlag = pendingArr && labels && level && id && label && child
  if (!tempFlag) return
  const ids = []
  let arr = JSON.parse(JSON.stringify(pendingArr))
  let currentIndex = 0
  do {
    for (let i = 0; i < arr.length; i++) {
      if (labels[currentIndex] === arr[i][label]) {
        ids.push(arr[i][id])
        if (currentIndex < level) {
          arr = arr[i][child]
        }
        break
      }
    }
    ++currentIndex
  } while (currentIndex < level)
  return ids.length === level ? ids : undefined
}

/**
 * 根据节点数组获取所需的节点信息
 *
 * @export
 * @param {array} pendingArr 待处理数组
 * @param {string} [idField='id'] id字段名称
 * @param {string} [childField='children'] children字段名称
 * @returns node 节点信息
 * @author duhh
 */
export function getFirstNodeIds(arr, idField = 'id', childField = 'children') {
  const tempFlag = arr instanceof Array && arr.length > 0 && idField && childField
  if (!tempFlag) return []
  const resultArr = []
  do {
    if (arr instanceof Array && arr.length > 0) {
      resultArr.push(arr[0][idField])
      arr = arr[0][childField]
    }
  } while (arr)
  return resultArr
}

/**
 * 判断节点是否存在
 * @param {*} arr 数组
 * @param {*} ids ids
 * @param {*} idField id字段名称
 * @param {*} childField children字段名称
 */
export function judgeNodeExistByIds(arr, ids, idField = 'id', childField = 'children') {
  let cloneIds
  if (typeof ids === 'string' || typeof ids === 'number') {
    cloneIds = [ids]
  } else if (Array.isArray(ids)) {
    cloneIds = [...ids]
  }
  const tempFlag = arr instanceof Array && arr.length > 0 && cloneIds && cloneIds.length > 0 && idField && childField
  if (!tempFlag) return false
  let _arr = JSON.parse(JSON.stringify(arr))
  let exist = false
  let current = 0
  do {
    exist = false
    if (_arr instanceof Array && _arr.length > 0) {
      for (let i = 0; i < _arr.length; i++) {
        if (_arr[i][idField] === cloneIds[current]) {
          exist = true
          _arr = _arr[0][childField]
          ++current
        }
      }
    }
  } while (_arr && exist && current < cloneIds.length)
  return exist
}

/**
 * 根据节点数组获取级联名称
 *
 * @export
 * @param {array} pendingArr 待处理数组
 * @param {array} ids ids 数组
 * @param {string} [idField='id'] id字段名称
 * @param {string} [childField='children'] children字段名称
 * @param {string} [nameField='name'] name字段名称
 * @returns node 节点信息
 * @author duhh
 */
export function getCascaderNameByIds(pendingArr, ids, idField = 'id', childField = 'children', nameField = 'name') {
  const tempFlag = pendingArr && ids && ids.length && idField && childField
  if (!tempFlag) return ''
  let arr = [...pendingArr]
  let currentIndex = 0
  let name = ''
  do {
    for (let i = 0; i < arr.length; i++) {
      if (ids[currentIndex] === arr[i][idField]) {
        name += arr[i][nameField]
        arr = arr[i][childField]
        break
      }
    }
    ++currentIndex
  } while (currentIndex < ids.length)
  return name
}

/**
 * 获取根到当前id的ids
 * @param {*} pendingTree 待处理树（数组）
 * @param {*} id 某节点的id
 * @param {*} idField id的字段名
 * @param {*} childField child的字段名
 * @param {*} ids 返回的ids数组
 */
export function getNodeIdsById(pendingTree, id, idField = 'id', childField = 'children', ids = []) {
  if (!pendingTree || pendingTree.length === 0) {
    return []
  } else {
    for (const item of pendingTree) {
      if (item[idField] === id) {
        ids.push(item[idField])
        return ids
      } else if (item[childField] && item[childField].length > 0) {
        ids.push(item[idField])
        ids = getNodeIdsById(item[childField], id, idField, childField, ids)
        return ids
      }
    }
    return []
  }
}

/**
 * 将空数组设置为undefined
 * @param {*} tree 数组（树）
 * @param {*} childField childField 字段名
 */
export function setEmptyArr2Undefined(tree, childField = 'children') {
  if (tree && tree.length > 0) {
    tree.forEach((node) => {
      if (node[childField]) {
        if (node[childField].length < 1) {
          node[childField] = undefined
        } else {
          setEmptyArr2Undefined(node[childField], childField)
        }
      }
    })
  }
}

export function filterTreeNode(tree, childField, field, value, pollingFloor = 1) {
  let treeCopy
  if (pollingFloor === 1) {
    treeCopy = deepClone(tree)
  } else {
    treeCopy = tree
  }
  return treeCopy.filter((n) => {
    if (n[field] === value) {
      return false
    } else if (n[childField] && n[childField].length) {
      n[childField] = filterTreeNode(n[childField], childField, field, value, ++pollingFloor)
      if (!n[childField] || n[childField].length === 0) {
        delete n[childField]
      }
      return true
    } else {
      return true
    }
  })
}

// 查找一个节点的所有父节点
// TODO: getNodeIdsById 方法好像重复了
export function familyTree(arr1, id) {
  var temp = []
  var forFn = function (arr, id) {
    for (var i = 0; i < arr.length; i++) {
      var item = arr[i]
      if (item.id === id) {
        temp.push(item)
        forFn(arr1, item.pid)
        break
      } else {
        if (item.children) {
          forFn(item.children, id)
        }
      }
    }
  }
  forFn(arr1, id)
  return temp
}
// 查找一个树的第一个节点，深度遍历
export function getFirstNode(tree) {
  var temp = []
  var forFn = function (arr) {
    if (arr && arr.length > 0) {
      temp.push(arr[0])
      if (arr[0].children) {
        forFn(arr[0].children)
      }
    }
  }
  forFn(tree)
  return temp
}

// 获取所有的子节点id
export function getChildIds(tree) {
  const ids = []
  if (Array.isArray(tree)) {
    tree.forEach((node) => {
      if (node.children) {
        const childIds = getChildIds(node.children)
        return ids.push.apply(ids, childIds)
      } else {
        ids.push(node.id)
      }
    })
  }
  return ids
}

// 根据保留的节点，移除其他节点
export function removeNodeByExistIds(tree, ids) {
  if (!Array.isArray(tree) || !Array.isArray(ids)) return []
  return tree.filter((node) => {
    const exist = ids.includes(node.id)
    if (exist && node.children) {
      const children = removeNodeByExistIds(node.children, ids)
      node.children = isNotBlank(children) ? children : void 0
    }
    return exist
  })
}

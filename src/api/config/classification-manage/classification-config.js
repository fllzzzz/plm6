import request from '@/utils/request'

// 材料类型科目树
export function getMatClsTree() {
  return request({
    module: 'config',
    url: 'classification/material/tree',
    method: 'get'
  })
}

/**
 * 获取科目树
 */
export function get() {
  return request({
    module: 'config',
    url: 'classification/tree',
    method: 'get'
  })
}

/**
 * 批量添加 data[]
 * @param {number} pid|required 一级：0
 * @param {string} name|required 名称
 * @param {string} code 代码 一级必填（固定2位长度）
 * @param {number} attribute 材料属性 [1-4] 一级分类必填
 */
export function batchAdd(data) {
  return request({
    module: 'config',
    url: 'classification',
    method: 'post',
    data
  })
}

/**
 * 批量保存 data[]
 * @param {number} id|required id
 * @param {number} pid|required 一级：0
 * @param {string} name|required 名称
 * @param {string} code 代码 一级必填（固定2位长度）
 * @param {number} attribute 材料属性 [1-4] 一级分类必填
 */
export function save(data) {
  return request({
    module: 'config',
    url: 'classification',
    method: 'put',
    data
  })
}

/**
 * 批量删除 ids[]
 */
export function del(ids) {
  return request({
    module: 'config',
    url: 'classification',
    method: 'delete',
    data: ids
  })
}

export default { get, batchAdd, save, del }


import request from '@/utils/request'

/**
 * 获取科目规格
 */
export function get(params) {
  return request({
    module: 'config',
    url: `classification/specification/${params.id}`,
    method: 'get'
  })
}

/**
 * 添加规格
 * @param {number} subjectId|required 科目id
 * @param {string} name|required 名称
 * @param {boolean} isWeightMean|required 是否计算加权平均价
 * @param {Array} details [{code, value}]
 */
export function add(data) {
  return request({
    module: 'config',
    url: 'classification/specification',
    method: 'post',
    data
  })
}

/**
 * 编辑规格
 * @param {number} id|required id
 * @param {string} name|required 名称
 * @param {boolean} isWeightMean|required 是否计算加权平均价
 * @param {Array} details [{id, code, value}]
 */
export function edit(data) {
  return request({
    module: 'config',
    url: 'classification/specification',
    method: 'put',
    data
  })
}

/**
 * 删除规格名称
 */
export function del(ids) {
  return request({
    module: 'config',
    url: `classification/specification`,
    method: 'delete',
    data: ids
  })
}

/**
 * 批量删除规格详情
 */
export function delSpecification(ids) {
  return request({
    module: 'config',
    url: 'classification/specification/detail',
    method: 'delete',
    data: ids
  })
}

export default { get, del, edit, add }

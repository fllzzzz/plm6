
import request from '@/utils/request'

/**
 * 获取型材规格
 * @param {number} id 型材id
 * @returns
 */
export function get(params) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/specification',
    method: 'get',
    params
  })
}

// 添加型材信息
export function add(data) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/specification',
    method: 'post',
    data
  })
}

// 批量添加型材信息
export function batchAdd({ sectionSteelId, list }) {
  return request({
    module: 'config',
    url: `classification/material/section-steel/${sectionSteelId}/specification/batch`,
    method: 'post',
    data: list
  })
}

// 修改型材信息
export function edit(data) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/specification',
    method: 'put',
    data
  })
}

// 修改型材信息
export function del(ids) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/specification',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del, batchAdd }

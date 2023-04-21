import request from '@/utils/request'

/**
 * 获取工序列表
 */
export function get(params) {
  return request({
    url: 'api/mes/enclosure/process/group',
    method: 'get',
    params
  })
}

/**
 * 编辑工序上报类型
 * @param {number} id|required id
 * @param {number} inspectType|required 质检类型
 * @param {number} reportType|required 上报类型
 */
export function edit(data) {
  return request({
    url: '/api/mes/enclosure/process',
    method: 'put',
    data
  })
}

export default { get, edit }

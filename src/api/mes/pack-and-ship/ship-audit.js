import request from '@/utils/request'

/**
 * 发运审核列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'cargo/review',
    method: 'get',
    params
  })
}

/**
 * 发运审核详情
 * @param {number} id
 */
export function detail(id) {
  return request({
    module: 'mes',
    url: `cargo/review/${id}`,
    method: 'get'
  })
}

/**
 * 审核
 * @param {number} id|required 发运信息id
 * @param {number} status|required 审核状态（ 1 || 2 ）
 */
export function audit({ id, status }) {
  return request({
    module: 'mes',
    url: 'cargo/review',
    method: 'put',
    data: { id, status }
  })
}

/**
 * 下载发运详情
 * @param {*} id 文件id
 */
export function download({ id }) {
  return request({
    module: 'mes',
    url: `cargo/review/${id}/download`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get, download }

import request from '@/utils/request'

/**
 *施工日志列表
 * @export
 * @returns
 */
export function get(params) {
  return request({
    module: 'project',
    url: 'data/construction/log',
    method: 'get',
    params
  })
}

/**
 *新增施工日志
 * @export
 * @returns
 */
export function add(data) {
  return request({
    module: 'project',
    url: 'data/construction/log',
    method: 'post',
    data
  })
}

export default { get, add }

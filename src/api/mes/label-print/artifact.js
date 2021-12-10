import request from '@/utils/request'

/**
 * 构件标签列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'print/artifact',
    method: 'get',
    params
  })
}

export default { get }

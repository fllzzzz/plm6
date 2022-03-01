import request from '@/utils/request'

/**
 * 配套件标签列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'print/auxiliary-material',
    method: 'get',
    params
  })
}

export default { get }

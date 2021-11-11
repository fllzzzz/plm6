import request from '@/utils/request'

/**
 * @description: 组立可排产[一次排产]列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'assemble/scheduling/page',
    method: 'get',
    params
  })
}

export default { get }

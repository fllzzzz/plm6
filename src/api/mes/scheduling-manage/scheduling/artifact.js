import request from '@/utils/request'

/**
 * @description: 构件可排产[二次排产]列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/page',
    method: 'get',
    params
  })
}

export default { get }

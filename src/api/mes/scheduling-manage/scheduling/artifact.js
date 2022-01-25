import request from '@/utils/request'

/**
 * @description: 构件可工单[二次工单]列表
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

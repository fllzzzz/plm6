import request from '@/utils/request'

/**
 * @description: 组立可工单[一次工单]列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/assemble/page',
    method: 'get',
    params
  })
}

export default { get }

import request from '@/utils/request'

/**
 * @description:组立工单汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/assemble/summary',
    method: 'get',
    params
  })
}

/**
 * @description:构件工单汇总
 */
export function getArtifact(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/summary',
    method: 'get',
    params
  })
}

export default { get }

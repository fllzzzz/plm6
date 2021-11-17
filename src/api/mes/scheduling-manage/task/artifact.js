import request from '@/utils/request'

/**
 * @description:组立排产汇总
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
 * @description:构件排产汇总
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

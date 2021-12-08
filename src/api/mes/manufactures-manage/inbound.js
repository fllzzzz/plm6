import request from '@/utils/request'

/**
 *
 * 获取构件汇总信息
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function getSummaryForArtifact({ monomerId, factoryId }) {
  return request({
    url: 'api/mes/inbound/state/artifact/summary',
    method: 'get',
    params: { monomerId, factoryId }
  })
}

/**
 *
 * 获取围护汇总信息
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function getSummaryForEnclosure({ monomerId, factoryId }) {
  return request({
    url: 'api/mes/inbound/state/enclosure/summary',
    method: 'get',
    params: { monomerId, factoryId }
  })
}


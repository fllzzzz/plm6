import request from '@/utils/request'

/**
 *
 * 获取构件汇总信息
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function getSummary({ monomerId, factoryId }) {
  return request({
    url: 'api/mes/warehouse/state/artifact/summary',
    method: 'get',
    params: { monomerId, factoryId }
  })
}

/**
 *
 * 获取构件汇总信息
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function download({ monomerId, factoryId }) {
  return request({
    url: 'api/mes/warehouse/state/artifact/download',
    method: 'get',
    responseType: 'blob',
    params: { monomerId, factoryId }
  })
}

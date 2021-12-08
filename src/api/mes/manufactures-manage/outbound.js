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
    url: 'api/mes/outbound/state/artifact/summary',
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
    url: 'api/mes/outbound/state/enclosure/summary',
    method: 'get',
    params: { monomerId, factoryId }
  })
}

/**
 *
 * 获取辅材汇总信息
 * @export
 * @param {*} monomerId|required 单体id
 * @returns
 */
export function getSummaryForAuxiliaryMaterial({ monomerId }) {
  return request({
    url: 'api/mes/outbound/state/auxiliaryMaterial/summary',
    method: 'get',
    params: { monomerId }
  })
}

/**
 *
 * 获取辅材看板
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @param {*} areaId|required 区域id
 * @param {*} firstId 物料一级分类id
 * @param {*} secondId 物料二级类型id
 * @param {*} thirdId 物料三级材质id
 * @param {*} specification 规格
 * @param {*} color 颜色
 * @returns
 */
export function getBoardForAuxiliaryMaterial({ page, size }) {
  return request({
    url: 'api/mes/outbound/state/auxiliaryMaterial/dashboard',
    method: 'get',
    cancelKey: 'api/mes/outbound/state/auxiliaryMaterial/dashboard',
    params: { page, size }
  })
}

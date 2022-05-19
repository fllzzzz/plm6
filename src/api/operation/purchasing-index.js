import request from '@/utils/request'

/**
 *  采购占比分析
 * @param {string} dateTime
 * @param {number} basicClass
 * @returns
 */
export function getPurchaseRatio(params) {
  return request({
    url: `/api/operational/analysis/purchase/ratio`,
    method: 'get',
    params: params,
    cancelKey: false
  })
}

/**
 *  采购趋势分析
 * @param {string} dateTime
 * @param {number} basicClass
 * @param {number} classifyId
 * @param {number} sectionSteelSpecId
 * @param {number} specId
 * @param {number} thickness
 * @param {number} range
 * @returns
 */
export function getPurchaseTrend({ dateTime, basicClass, classifyId, sectionSteelSpecId, specId, thickness, range }) {
  return request({
    url: `/api/operational/analysis/purchase/trend`,
    method: 'get',
    params: { dateTime, basicClass, classifyId, sectionSteelSpecId, specId, thickness, range },
    cancelKey: false
  })
}


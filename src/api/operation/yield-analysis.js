import request from '@/utils/request'

/**
 *  产量分析
 * @param {string} dateTime
 * @param {number} factoryId
 * @param {number} productType
 * @param {number} yearSize
 * @returns
 */
export function getYieldAnalysis(params) {
  return request({
    url: `/api/operational/analysis/production`,
    method: 'get',
    params: params,
    cancelKey: false
  })
}


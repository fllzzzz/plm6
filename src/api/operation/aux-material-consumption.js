import request from '@/utils/request'

/**
 * 辅材消耗
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getAuxAnalysis(params) {
  return request({
    url: `/api/operational/analysis/auxiliary_materials`,
    method: 'get',
    params: params
  })
}


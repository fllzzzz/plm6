import request from '@/utils/request'

/**
 * 检验合格率
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getInspectionQualifiedRate(params) {
  return request({
    url: `/api/operational/analysis/inspection`,
    method: 'get',
    params: params
  })
}


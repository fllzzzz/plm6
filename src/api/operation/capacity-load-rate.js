import request from '@/utils/request'

/**
 * 产能负荷率
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getCapacityLoadRate(params) {
  return request({
    url: `/api/operational/analysis/load`,
    method: 'get',
    params: params
  })
}


import request from '@/utils/request'

/**
 * 退库明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/return/details`,
    method: 'get',
    params
  })
}

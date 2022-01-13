import request from '@/utils/request'

/**
 * 出库明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/outbound/details`,
    method: 'get',
    params
  })
}

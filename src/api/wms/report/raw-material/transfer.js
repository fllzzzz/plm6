import request from '@/utils/request'

/**
 * 调拨明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/transfer/details`,
    method: 'get',
    params
  })
}

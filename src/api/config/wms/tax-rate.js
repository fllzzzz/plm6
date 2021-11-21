import request from '@/utils/request'

/**
 * 税率列表
 *
 * @export
 * @returns
 */
export function getTaxRateBrief(params) {
  return request({
    module: 'wms',
    url: 'config/material/tax-rate/all/brief',
    method: 'get',
    params
  })
}

/**
 *税率列表
 *
 * @export
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'config/material/tax-rate',
    method: 'get',
    params
  })
}

/**
 *编辑税率
 *
 * @export
 * @param
 * @returns
 */
export function edit(data) {
  return request({
    module: 'wms',
    url: 'config/material/tax-rate',
    method: 'put',
    data
  })
}

export default { get, edit }

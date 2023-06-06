import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'warehouse/finish-product/page',
    method: 'get',
    params
  })
}

export function detail(params) {
  return request({
    module: 'bridge',
    url: 'warehouse/finish-product/detail/page',
    method: 'get',
    params
  })
}

export function summaryData(params) {
  return request({
    module: 'bridge',
    url: 'warehouse/finish-product/summary',
    method: 'get',
    params
  })
}

// 清单 出库 入库 库存 详情
export function artifactProductDetail(params) {
  return request({
    module: 'bridge',
    url: 'warehouse/report/finish-product/box',
    method: 'get',
    params
  })
}
export default { get }

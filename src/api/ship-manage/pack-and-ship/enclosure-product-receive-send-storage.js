import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/enclosure/page',
    method: 'get',
    params
  })
}

export function detail(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/detail/enclosure/page',
    method: 'get',
    params
  })
}

export function summaryData(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/enclosure/summary',
    method: 'get',
    params
  })
}

// 清单 出库 入库 库存 详情
export function enclosureProductDetail(params) {
  return request({
    url: '/api/mes/building/warehouse/report/finish-product/enclosure',
    method: 'get',
    params
  })
}
export default { get }

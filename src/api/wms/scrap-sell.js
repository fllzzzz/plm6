import request from '@/utils/request'

// 获取价格录入类型
export function getPriceType(params) {
  return request({
    url: '/api/config/getWastePriceEntryType/public',
    method: 'get',
    params
  })
}

// 获取废料出售列表
export function get(params) {
  return request({
    module: 'wms',
    url: 'report/wasteSale',
    method: 'get',
    params
  })
}

// 获取出售单位
export function getSellUnit(params) {
  return request({
    url: '/api/branchCompany/all/simple',
    method: 'get',
    params
  })
}

// 获取购买单位
export function getBuyUnit(params) {
  return request({
    url: '/api/contract/contractWaste/listAll',
    method: 'get',
    params
  })
}

// 新增废料出售
export function addScrapSell(data) {
  return request({
    module: 'wms',
    url: 'report/wasteSale',
    method: 'post',
    data
  })
}

// 修改废料出售
export function editScrapSell(data) {
  return request({
    module: 'wms',
    url: 'report/wasteSale',
    method: 'put',
    data
  })
}

// 删除废料出售
export function delScrapSell(data) {
  return request({
    module: 'wms',
    url: 'report/wasteSale',
    method: 'delete',
    data
  })
}

// 废料出售审核
export function checkScrapSell(data) {
  return request({
    module: 'wms',
    url: 'report/wasteSale/check',
    method: 'put',
    data
  })
}

// 废料详情下载
export function downloadScrap(id) {
  return request({
    module: 'wms',
    url: `report/wasteSale/downLoad/${id}`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get }

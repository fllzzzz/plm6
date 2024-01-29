import request from '@/utils/request'

// 废料类型列表
export function getScrapTypeList(params) {
  return request({
    module: 'wms',
    url: 'config/wasteClassification/all',
    method: 'get',
    params
  })
}

// 废料合同配置列表
export function getScrapContractList(params) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste',
    method: 'get',
    params
  })
}

// 新增废料合同配置
export function addScrapContractList(data) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste',
    method: 'post',
    data
  })
}

// 修改废料合同配置
export function editScrapContractList(data) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste',
    method: 'put',
    data
  })
}

// 删除废料合同配置
export function delScrapContractList(data) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste',
    method: 'delete',
    data
  })
}

export function getBuyList(params) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste/summary/ledger',
    method: 'get',
    params
  })
}

// 按日期查询
export function getDateList(params) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste/saleDetail',
    method: 'get',
    params
  })
}

// 累计金额查询列表
export function totalAmountList(params) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste/saleAmount',
    method: 'get',
    params
  })
}

// 累计出售金额
export function getTotalAmount(params) {
  return request({
    module: 'contract',
    url: 'contract/contractWaste/saleTotalAmount',
    method: 'get',
    params
  })
}

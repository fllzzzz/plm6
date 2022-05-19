import request from '@/utils/request'

// 获取每月发运量
export function getCargoMete(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/amount',
    method: 'get',
    params: params
  })
}

// 获取当年车型使用统计
export function getModelList(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/model',
    method: 'get',
    params: params
  })
}

// 获取当年车型使用统计
export function getProjectDay(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/project/day',
    method: 'get',
    params: params
  })
}

// 获取当年车型使用统计
export function getProjectYear(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/project/year',
    method: 'get',
    params: params
  })
}

// 获取每月发运车次
export function getCargoQuantity(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/quantity',
    method: 'get',
    params: params
  })
}

// 获取当年发运量
export function getCargoSummary(params) {
  return request({
    module: 'mes',
    url: 'kpi/cargo_list/summary',
    method: 'get',
    params: params
  })
}
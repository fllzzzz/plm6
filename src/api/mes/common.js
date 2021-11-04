import request from '@/utils/request'

// 获取所有工厂
export function getFactoriesAllSimple(params) {
  return request({
    module: 'mes',
    url: 'factory',
    method: 'get',
    params
  })
}

// 获取所有车间
export function getWorkshopAllSimple(params) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'get',
    params
  })
}

// 获取所有工序
export function getProcessAllSimple(params) {
  return request({
    url: 'api/mes/building/process',
    method: 'get',
    params
  })
}

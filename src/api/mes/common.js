import request from '@/utils/request'

// 获取所有工厂
export function getFactoriesAllSimple(params) {
  return request({
    module: 'mes',
    url: 'factory',
    method: 'get',
    params,
    cancelKey: false
  })
}

// 获取所有车间
export function getWorkshopsAllSimple(params) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'get',
    params,
    cancelKey: false
  })
}

// 获取所有工序
export function getProcessAllSimple(params) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'get',
    params,
    cancelKey: false
  })
}

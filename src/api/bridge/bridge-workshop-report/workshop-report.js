import request from '@/utils/request'

/**
 * @description: 获取车间报表清单-分页
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'workshop/box/summary/page',
    method: 'get',
    params
  })
}
/**
 * @description: 全年累计产量
 */
export function fullYearProduction(params) {
  return request({
    module: 'bridge',
    url: 'workshop/year/mete/summary',
    method: 'get',
    params
  })
}
/**
 * @description: 获取车间报表
 */
export function workshopEcharts(params) {
  return request({
    module: 'bridge',
    url: 'workshop/report',
    method: 'get',
    params
  })
}
/**
 * @description: 获取车间产量汇总
 */
export function workshopProduction(params) {
  return request({
    module: 'bridge',
    url: 'workshop/mete/summary',
    method: 'get',
    params
  })
}

export default { get }

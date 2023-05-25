import request from '@/utils/request'

/**
 * 车间报表清单-分页
 * @export
 * @param {string} startTime|required 开始时间
 * @param {string} endTime|required 结束时间
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function get(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/detail',
    method: 'get',
    params
  })
}

/**
 * 全年累计产量
 * @export
 * @param {string} time|required 时间戳
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function fullYearProduction(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/total',
    method: 'get',
    params
  })
}

/**
 * 车间报表图标数据
 * @export
 * @param {string} time|required 时间戳
 * @param {number} type|required 时间类型 1:当年 2:当月
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function workshopEcharts(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/list',
    method: 'get',
    params
  })
}

export default { get }

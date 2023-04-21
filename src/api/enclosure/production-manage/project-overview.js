import request from '@/utils/request'

/**
 * 获取所有项目及排产状态
 * @export
 * @param {string} year 年份
 * @param {string} name 项目模糊查询
 * @param {number} state 排产状态 1: 未排产 2:部分排产 4:已完全排产
 * @returns
 */
export function projectList(params) {
  return request({
    module: 'enclosure',
    url: 'overview/project',
    method: 'get',
    params
  })
}

/**
 * 获取所有项目下全部的围护种类
 * @export
 * @param {number} projectId|required 项目id
 * @returns
 */
export function categoryList(params) {
  return request({
    module: 'enclosure',
    url: 'overview/project/category',
    method: 'get',
    params
  })
}

/**
 * 依据项目和板材类型获取批次
 * @export
 * @param {number} projectId|required 项目id
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function areaList(params) {
  return request({
    module: 'enclosure',
    url: 'overview/plans',
    method: 'get',
    params
  })
}

/**
 * 获取围护项目和区域待排产量
 * @export
 * @param {number} projectId|required 项目id
 * @param {array} planIds|required 批次id
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function enclosureSummary(params) {
  return request({
    module: 'enclosure',
    url: 'overview/product/total',
    method: 'get',
    params
  })
}

/**
 * 依据板材类型和批次获取全部的待排产数据
 * @export
 * @param {*} name|required 页码
 * @param {*} size|required 页大小
 * @param {array} planIds|required 批次id
 * @param {number} projectId|required 项目id
 * @param {number} category|required 围护类型 2压型(彩)板 4夹芯板 8桁架楼承板 16压型楼承板 32折边件
 * @returns
 */
export function get(params) {
  return request({
    module: 'enclosure',
    url: 'overview/product/detail',
    method: 'get',
    params
  })
}

export default { get }

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
    url: 'scheduling/project',
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
    url: 'scheduling/category',
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
    url: 'scheduling/plans',
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
    url: 'scheduling/totalLength',
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
    url: 'scheduling/list',
    method: 'get',
    params
  })
}

/**
 * 生产排产-任务下发
 * @export
 * @param {number} projectId|required 项目id
 * @param {number} factoryId|required 工厂id
 * @param {number} workshopId|required 车间id
 * @param {number} productionLineId|required 生产线id
 * @param {number} teamId|required 班组id
 * @param {array} schedulings|required 排产信息[{productId, quantity}]
 * @param {number} askCompleteTime|required 完成日期
 * @param {number} category|required 围护类型
 * @returns
 */
export function add(data) {
  return request({
    module: 'enclosure',
    url: 'scheduling',
    method: 'post',
    data
  })
}

export default { get, add }

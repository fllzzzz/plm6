
import request from '@/utils/request'

/**
 * @description: 分段可排产列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/page',
    method: 'get',
    params
  })
}

/**
 * @description: 分段可排产生产线类型
 */
export function getLineType(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/line/type/list',
    method: 'get',
    params
  })
}

/**
 * @description: 获取分段排产汇总信息
 */
export function getSummary(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 保存分段排产
 */
export function save(data) {
  return request({
    module: 'bridge',
    url: 'scheduling/box',
    method: 'post',
    data
  })
}

/**
 * @description: 分段排产预览详情
 */
export function record(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取分段预排产汇总信息
 */
export function recordSummary(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 获取计算下发分段所需单元件数量列表
 */
export function getElement(data) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/element',
    method: 'post',
    data
  })
}

/**
 * @description: 编辑分段排产预览详情
 */
export function editRecord(data) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record',
    method: 'put',
    data
  })
}

/**
 * @description: 删除分段排产预览详情
 */
export function delRecord(ids) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record',
    method: 'delete',
    data: ids
  })
}

/**
 * 排程信息：层级：项目-单体-区域
 * @export
 * @returns
 */
export function getProjectToAreaTree(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/tree/list',
    method: 'get',
    params
  })
}

/**
 * 获取排程信息可选时间
 * @export
 * @returns
 */
export function getAreaTreeTime(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/area/tree/time',
    method: 'get',
    params
  })
}

/**
 * 根据生产线类型获取分段排产类型
 * @export
 * @returns
 */
export function getBoxRecordType(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record/type/list',
    method: 'get',
    params
  })
}

/**
 * 获取生产线类型
 * @export
 * @returns
 */
export function getLineRecordType(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/record/line/type/list',
    method: 'get',
    params
  })
}

/**
 * 根据生产线类型获取分段排产类型
 * @export
 * @returns
 */
export function getBoxType(params) {
  return request({
    module: 'bridge',
    url: 'scheduling/box/type/list',
    method: 'get',
    params
  })
}

/**
 * 获取 车间-产线-生产组
 * @export
 * @returns
 */
export function getGroupsTree(params) {
  return request({
    module: 'bridge',
    url: 'groups/scheduling/tree',
    method: 'get',
    params
  })
}

export default { get }



import request from '@/utils/request'

/**
 * @description: 构件可排产列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/page',
    method: 'get',
    params
  })
}

/**
 * @description: 构件可排产生产线类型
 */
export function getLineType(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/line/type/list',
    method: 'get',
    params
  })
}

/**
 * @description: 获取构件排产汇总信息
 */
export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 保存构件排产
 */
export function save(data) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact',
    method: 'post',
    data
  })
}

/**
 * @description: 构件排产预览详情
 */
export function record(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/record/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取构件预排产汇总信息
 */
export function recordSummary(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/record/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 获取计算下发构件所需部件数量列表
 */
export function getAssemble(data) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/assemble',
    method: 'post',
    data
  })
}

/**
 * @description: 编辑构件排产预览详情
 */
export function editRecord(data) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/record',
    method: 'put',
    data
  })
}

/**
 * @description: 删除构件排产预览详情
 */
export function delRecord(ids) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/record',
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
    module: 'mes',
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
    module: 'mes',
    url: 'scheduling/area/tree/time',
    method: 'get',
    params
  })
}

/**
 * 根据生产线类型获取构件排产类型
 * @export
 * @returns
 */
export function getArtifactRecordType(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/record/type/list',
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
    module: 'mes',
    url: 'scheduling/artifact/record/line/type/list',
    method: 'get',
    params
  })
}

/**
 * 根据生产线类型获取构件排产类型
 * @export
 * @returns
 */
export function getArtifactType(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/type/list',
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
    module: 'mes',
    url: 'groups/scheduling/tree',
    method: 'get',
    params
  })
}

export default { get }


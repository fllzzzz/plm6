
import request from '@/utils/request'

/**
 * @description: 零件可排产列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/list',
    method: 'get',
    params
  })
}

// 获取可排产的月份
export function getMonth(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/month/list',
    method: 'get',
    params
  })
}

export function getDate(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/date/list',
    method: 'get',
    params
  })
}

export function getProject(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/project/tree',
    method: 'get',
    params
  })
}

export function getTypeList(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/type/list',
    method: 'get',
    params
  })
}

export function getMaterial(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/material/list',
    method: 'get',
    params
  })
}

export function getThick(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/thick/list',
    method: 'get',
    params
  })
}

export function getLayingWay(params) {
  return request({
    module: 'mes',
    url: 'cut/list/laying',
    method: 'get',
    params
  })
}

/**
 * @description: 保存零件排产
 */
export function save(data) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling',
    method: 'post',
    data
  })
}

/**
 * @description: 保存零件排产-新版
 */
export function newSave(data) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/nest_cut',
    method: 'post',
    data
  })
}

/**
 * @description: 删除零件排产
 */
export function del(ids) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling',
    method: 'delete',
    data: ids
  })
}

/**
 * @description: 删除零件排产详情
 */
export function delDetail(id) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/detail/${id}`,
    method: 'delete'
  })
}

/**
 * @description: 零件排产记录
 */
export function record(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/record/page',
    method: 'get',
    params
  })
}

/**
 * @description: 零件排产记录详情
 */
export function recordDetail(id) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/record/${id}`,
    method: 'get'
  })
}

/**
 * @description:【套料下发】
 */
export function saveNesting(data) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/nesting',
    method: 'post',
    data
  })
}

/**
 * @description:获取可排产的切割任务单
 */
export function getNestingTask(params) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/task/page',
    method: 'get',
    params
  })
}

/**
 * @description:获取可排产的切割任务单详情
 */
export function getNestingTaskDetail({ id }) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/task/${id}`,
    method: 'get'
  })
}

/**
 * @description:获取可排产的切割任务单详情
 */
export function getCutTaskDetail(params) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/task/list`,
    method: 'get',
    params
  })
}

/**
 * @description:无需套料保存前判断是否需要钻孔
 */
export function getHoleTaskDetail(data) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/check/drill`,
    method: 'post',
    data
  })
}

/**
 * @description: 获取套料排产的项目列表
 */
export function getProjectTaskDetail(params) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/nest_cut/project/list`,
    method: 'get',
    params
  })
}

/**
 * @description: 任务单下零件的详细信息(任务单零件列表)
 */
export function getMachinePartListDetail(id) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/record/${id}`,
    method: 'get'
  })
}
/**
 * @description: 任务下零件详情(项目,单体数据查询)
 */
export function getProjectDetail(params) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/project/distribution`,
    method: 'get',
    params
  })
}
/**
 * @description: 获取任务单pdf
 */
export function getShowPdf(params) {
  return request({
    module: 'mes',
    url: `machine_part/scheduling/pdf`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default { get }


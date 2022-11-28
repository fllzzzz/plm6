
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
    url: 'machine_part/scheduling/project/list',
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

export default { get }


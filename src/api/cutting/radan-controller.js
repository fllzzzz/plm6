import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/radan/getPartByProjectId',
    method: 'get',
    params
  })
}

// 项目id查看项目零件
export function getTaskByProjectId(params) {
  return request({
    url: `/api/radan/getTaskByProjectId`,
    method: 'get',
    params
  })
}

// 套料任务项目信息总览
export function getProjectInfo() {
  return request({
    url: `/api/radan/getProjectInfo`,
    method: 'get',
  })
}

// 创建任务包、根据厚度（翼板副板分）
export function creatTaskPack(data, params) {
  return request({
    url: `/api/radan/creatTaskPack`,
    method: 'post',
    data,
    params
  })
}

// 查看任务包
export function getTaskPack(params) {
  return request({
    url: `/api/radan/getTaskPack`,
    method: 'get',
    params
  })
}

// 投料操作
export function feedingOperation(data, params) {
  return request({
    url: `/api/order/feedingOperation`,
    method: 'post',
    data,
    params
  })
}

// 上传任务包
export function uploadTask({
  cutTaskId
}) {
  return request({
    url: `/api/radan/uploadTask/${cutTaskId}`,
    method: 'get'
  })
}

// 任务id查零件清单
export function ByCutTaskId(params) {
  return request({
    url: `/api/radan/getPartByCutTaskId`,
    method: 'post',
    params
  })
}

// 区域id 查零件清单
export function ByAreaId(params) {
  return request({
    url: `/api/order/getPartByAreaId/${params}`,
    method: 'get'
  })
}

// 单体
export function monomer(params) {
  return request({
    url: `/api/plan/monomer`,
    method: 'get',
    params
  })
}

// 区域
export function area(params) {
  return request({
    url: `/api/plan/area`,
    method: 'get',
    params
  })
}

// 创建套料工单
export function createOrder(data, params) {
  return request({
    url: `/api/order/createOrder`,
    method: 'post',
    data,
    params
  })
}

export default {
  get,
  area,
  ByAreaId,
  uploadTask,
  ByCutTaskId,
  getTaskPack,
  creatTaskPack,
  feedingOperation,
  getTaskByProjectId
}

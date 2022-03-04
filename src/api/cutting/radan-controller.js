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

// 上传任务包
export function uploadTask({
  cutTaskId
}) {
  return request({
    url: `/api/radan/uploadTask/${cutTaskId}`,
    method: 'get'
  })
}

export function ByCutTaskId(params) {
  return request({
    url: `/api/radan/getPartByCutTaskId`,
    method: 'post',
    params
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

export default {
  get,
  area,
  getTaskByProjectId,
  creatTaskPack,
  getTaskPack,
  uploadTask,
  ByCutTaskId
}
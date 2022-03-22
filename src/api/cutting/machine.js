import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/cut/getMachine',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/cut/addMachine',
    method: 'post',
    data
  })
}

export function del(ids) {
  return request({
    url: `api/cut/deleteMachine/${ids}`,
    method: 'delete'
  })
}

// 分配任务
export function assign(params, data) {
  return request({
    url: `/api/cut/assign/`,
    method: 'post',
    params,
    data
  })
}

export function sentTask(data) {
  return request({
    url: `/api/cut/sentTask`,
    method: 'post',
    data
  })
}

// 查看单机已下发
export function getTaskByMac(params) {
  return request({
    url: `/api/cut/getTaskByMac/${params}`,
    method: 'get'
  })
}

// nc文件查钢板
export function getPlantByFileName(params) {
  return request({
    url: `/api/cut/getPlantByFileName/${params}`,
    method: 'get'
  })
}
// 清除任务
export function cleanTask(params, data) {
  return request({
    url: `/api/cut/cleanTask`,
    method: 'post',
    params,
    data
  })
}

// 转产任务
export function changeTask(params, data) {
  return request({
    url: `/api/cut/changeTask`,
    method: 'post',
    params,
    data
  })
}

// 监控
export function getMachineInformation(params) {
  return request({
    url: `/api/cut/getMachineInformation/${params}`,
    method: 'post'
  })
}

export default {
  get,
  add,
  del
}

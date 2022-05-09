import request from '@/utils/request'

// 获取切割设备列表
export function get(params) {
  return request({
    url: '/api/cut/getMachine',
    method: 'get',
    params
  })
}
// 通过mac地址查找设备
export function getMac(mac) {
  return request({
    url: `/api/cut/getMachine/${mac}`,
    method: 'get',
    cancelKey:false
  })
}

// 增加切割设备
export function add(data) {
  return request({
    url: '/api/cut/addMachine',
    method: 'post',
    data
  })
}

// 删除切割设备
export function del(ids) {
  return request({
    url: `api/cut/deleteMachine/${ids}`,
    method: 'delete'
  })
}
// 修改切割设备
export function edit(data) {
  return request({
    url: '/api/cut/updateMachine',
    method: 'post',
    data,
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

// 下发任务
export function sentTask(data) {
  return request({
    url: `/api/cut/sentTask`,
    method: 'post',
    data
  })
}

// 重置任务
export function resetTask(data) {
  return request({
    url: `/api/cut/resetTask`,
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
    method: 'get'
  })
}

export default {
  get,
  add,
  del,
  edit,
  getMac,
  sentTask,
  cleanTask,
  resetTask,
  getMachineInformation
}

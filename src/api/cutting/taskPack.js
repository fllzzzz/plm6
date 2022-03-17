import request from '@/utils/request'

// 查看任务包
export function get(params) {
  return request({
    url: `/api/radan/getTaskPack`,
    method: 'get',
    params
  })
}

// 撤销工单
export function del(params) {
  return request({
    url: `/api/order/deletesOrder/${params}`,
    method: 'delete'
  })
}

// 上传工单
export function uploadOrder(params) {
  return request({
    url: `/api/order/uploadOrder/${params}`,
    method: 'get'
  })
}

// 上传工单 修改工单按钮
export function updateOrder(params) {
  return request({
    url: `/api/order/updateOrder`,
    method: 'get',
    params
  })
}

// 新增零件到工单
export function addPartFromOrder(params, data) {
  return request({
    url: `/api/order/addPartFromOrder`,
    method: 'post',
    params,
    data
  })
}

// 新增零件到工单
export function deletePartFromOrder(params, data) {
  return request({
    url: `/api/order/deletePartFromOrder`,
    method: 'post',
    params,
    data
  })
}

export default {
  get,
  del,
  uploadOrder,
  updateOrder,
  addPartFromOrder,
  deletePartFromOrder
}

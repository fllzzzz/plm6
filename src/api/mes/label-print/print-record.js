
import request from '@/utils/request'

/**
 * 获取任务打印记录
 * @export
 * @param {number} taskId|required 任务id
 * @returns
 */
export function getForTask(taskId) {
  return request({
    module: 'mes',
    url: `print/record/task/${taskId}`,
    method: 'get'
  })
}

/**
 * 获取材料打印记录
 * @export
 * @param {number} materialId|required 材料id
 * @returns
 */
export function getForMaterial(materialId) {
  return request({
    module: 'mes',
    url: `print/record/material/${materialId}`,
    method: 'get'
  })
}

/**
 * 获取包单打印记录
 * @export
 * @param {number} id|required 包id
 * @returns
 */
export function getForPackage(id) {
  return request({
    module: 'mes',
    url: `print/record/package/${id}`,
    method: 'get'
  })
}

/**
 * 添加任务打印记录
 * @export
 * @param {number} taskId|required 任务id
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function taskAdd({ id, quantity, startTime, endTime }) {
  return request({
    module: 'mes',
    url: 'print/record/task',
    method: 'post',
    data: { taskId: id, quantity, startTime, endTime }
  })
}

/**
 * 添加材料打印记录
 * @export
 * @param {number} taskId|required 材料id
 * @param {number} materialListType|required 材料类型
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function materialAdd({ id, quantity, startTime, endTime }) {
  return request({
    module: 'mes',
    url: 'print/record/material',
    method: 'post',
    data: { materialId: id, quantity, startTime, endTime }
  })
}

/**
 * 添加包单打印记录
 * @export
 * @param {number} id|required 包id
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function packageRecordAdd({ id, quantity, startTime, endTime }) {
  return request({
    module: 'mes',
    url: 'print/record/package',
    method: 'post',
    data: { packId: id, quantity, startTime, endTime }
  })
}

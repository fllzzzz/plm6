import { parseTime } from '@/utils/date'
import { isBlank } from '@data-type/index'
import { projectStatusEnum, projectNameArrangementModeEnum } from '@enum-ms/contract'
import EO from '@enum'
import store from '@/store'

/**
 * 获取项目信息
 * @param {number} id 项目id
 */
export function getProjectInfo(id) {
  if (!id) return
  // 后期改KV模式
  const projects = store.getters.userProjects
  for (const project of projects) {
    if (project.id === id) {
      return project
    }
  }
  return
}

/**
 * 项目名称格式转化
 * TODO: 后期，服务端若配置简称，则全称不传
 * @param {object} project 项目 { id,name,serialNumber }
 * @param {object} config 配置
 * @param {boolean} lineBreak=[true] 是否换行,默认换行
 */
export function projectNameFormatter(project, config, lineBreak = true) {
  if (isBlank(project)) {
    return ''
  }
  // 若不传入配置，则调用系统配置
  const _config = store.getters.projectNameShowConfig
  if (isBlank(config)) {
    config = store.getters.projectNameShowConfig
  }

  // 打印表格取默认配置
  config.arrangement = config.arrangement || _config.arrangement

  const _projectName = config.showProjectFullName ? project.name : project.shortName
  if (!config.showSerialNumber || !project.serialNumber) {
    return _projectName
  }
  let extra = `   `
  if (lineBreak) {
    extra = ` \n`
  }

  // 默认全局配置
  switch (config.arrangement) {
    case projectNameArrangementModeEnum.SERIAL_NUMBER_START.V:
      return `${project.serialNumber}${extra}${_projectName}`
    case projectNameArrangementModeEnum.SERIAL_NUMBER_END.V:
      return `${_projectName}${extra}${project.serialNumber}`
    default:
      return `${_projectName}`
  }
}

/**
 * 将年份分组的项目转化为级联(树)
 * @param {Array} arr 项目数组
 * @param {string} timeField 时间字段
 * @author dhh
 */
export function projectsToCascade(projects, timeField = 'createTime') {
  const cascade = [] // 级联列表
  const statusArr = EO.toArr(projectStatusEnum) // 枚举转数组
  const statusValArr = statusArr.map((v) => v.V) // 枚举值数组
  const year = [] // 年份

  // 年份排序
  const compare = function (obj1, obj2) {
    // 年份排序 倒序
    const year1 = -obj1.id
    const year2 = -obj2.id
    return year2 - year1
  }

  // 遍历项目
  projects.forEach((project) => {
    const pYear = parseTime(project[timeField], '{y}')
    // 获取数组下标
    let yIndex = year.indexOf(pYear)
    // 当前年份不在数组中
    if (yIndex === -1) {
      year.push(pYear)
      // 设置级联第一级：项目年份
      cascade.push({
        id: -parseInt(pYear),
        name: pYear,
        children: []
      })
      // 更新数组下标
      yIndex = year.length - 1
      // 设置级联第二级：项目状态
      statusArr.forEach((item) => {
        cascade[yIndex].children.push({
          id: -Number(item.V),
          name: item.L,
          children: []
        })
      })
    }
    project.fullName = project.name
    project.name = projectNameFormatter(project)
    // 设置级联第三级：项目
    if (statusValArr.indexOf(project.status) > -1) {
      cascade[yIndex].children[statusValArr.indexOf(project.status)].children.push(project)
    }
  })
  // 级联项目排序
  cascade.sort(compare)
  return cascade
}

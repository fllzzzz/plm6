import { validatorPhone } from '@/utils/validate/pattern'
import configRouter from '@/router/modules/config'
import mesRouter from '@/router/modules/mes'
import projectRouter from '@/router/modules/project'
import wmsRouter from '@/router/modules/wms'

// 用户登录（获取token）
const userLogin = {
  url: '/api/user/login',
  method: 'post',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'token': 'TOKEN_MYJ_XCJ_CCJ_HHHHHH'
      }
    }
  }
}

// 获取用户信息
const userInfo = {
  url: '/api/user/info',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'id': 1,
        'name': '@cname',
        'username': '@name',
        'sex|0-1': 0,
        'phone': validatorPhone,
        'email': '@email',
        'companyName': '杭州初鸣建筑科技有限公司',
        'inventoryNotifyPerm': true,
        'roles': ['admin'],
        'roleNames': ['超级管理员'],
        'dept': '管理部门',
        'job': null,
        'permissions': [],
        'menus': [
          {
            'id': 475,
            'name': 'WMS',
            'icon': 'wms-wms',
            'redirect': 'wms'
          },
          {
            'id': 2,
            'name': '建刚MES',
            'icon': 'mes-steel',
            'redirect': 'mes-project'
          },
          {
            'id': 1,
            'name': '配置管理',
            'icon': 'config',
            'redirect': 'config-manage'
          },
          {
            'id': 4,
            'name': '项目管理',
            'icon': 'config',
            'redirect': 'project-manage/data-manage'
          },
          {
            'id': 3,
            'name': '成本管理',
            'icon': 'config',
            'redirect': 'base-config'
          }]
      }
    }
  }
}

// 用户登录（获取token）
const userLogout = {
  url: '/api/user/logout',
  method: 'delete',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 用户菜单
const userMenu = {
  url: '/api/user/menu',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          configRouter,
          projectRouter,
          wmsRouter,
          mesRouter,
          projectRouter
        ]
      }
    }
  }
}

export default [
  userLogin,
  userLogout,
  userInfo,
  userMenu
]

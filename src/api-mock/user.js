import { validatorPhone } from '@/utils/validate/pattern'
import configRouter from '@/router/modules/config'
import mesRouter from '@/router/modules/mes'
import projectRouter from '@/router/modules/project'
import wmsRouter from '@/router/modules/wms'
import planRouter from '@/router/modules/plan'
import contractRouter from '@/router/modules/contract'
// import supplyChainRouter from '@/router/modules/cutting'
import cuttingRouter from '@/router/modules/cutting'
import supplyChainRouter from '@/router/modules/supply-chain'
import userRouter from '@/router/modules/user'

// 用户登录（获取token）
const userLogin = {
  url: '/api/user/login',
  method: 'post',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        token: 'TOKEN_MYJ_XCJ_CCJ_HHHHHH'
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
        id: 1,
        name: '@cname',
        username: '@name',
        'sex|0-1': 0,
        phone: validatorPhone,
        email: '@email',
        companyName: '杭州初鸣建筑科技有限公司',
        inventoryNotifyPerm: true,
        roles: ['admin'],
        roleNames: ['超级管理员'],
        dept: '管理部门',
        job: null,
        permissions: [],
        menus: [
          {
            id: 3,
            name: '合同管理',
            icon: 'module-contract',
            redirect: 'contract'
          },
          {
            id: 6,
            name: '计划管理',
            icon: 'module-plan',
            redirect: 'plan'
          },
          {
            id: 2,
            name: '建刚MES',
            icon: 'module-steel',
            redirect: 'mes-project'
          },

          {
            id: 4,
            name: '项目管理',
            icon: 'module-project',
            redirect: 'project-manage'
          },
          {
            id: 5,
            name: 'WMS',
            icon: 'module-wms',
            redirect: 'wms'
          },
          {
            id: 7,
            name: '供应链',
            icon: 'module-scm',
            redirect: 'supply-chain'
          },
          {
            id: 8,
            name: 'BIM',
            icon: 'module-bim',
            redirect: 'bim'
          },
          {
            id: 821,
            // id: 171,
            name: '套料切割',
            icon: 'module-cutting',
            redirect: 'cutting'
          }, {
            id: 10000,
            // id: 171,
            name: '运营分析',
            icon: 'module-analysis',
            redirect: 'cutting'
          },
          {
            id: 794,
            name: '人员管理',
            icon: 'module-user',
            redirect: 'user-manage'
          },
          {
            id: 1,
            name: '配置管理',
            icon: 'module-config',
            redirect: 'config-manage'
          }
        ]
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
      data: [
        configRouter,
        projectRouter,
        wmsRouter,
        mesRouter,
        projectRouter,
        planRouter,
        contractRouter,
        supplyChainRouter,
        cuttingRouter,
        userRouter
      ]
    }
  }
}

export default [userLogin, userLogout, userInfo, userMenu]

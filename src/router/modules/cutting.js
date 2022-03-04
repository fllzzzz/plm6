// 路由：套料切割
export default {
  id: 171,
  name: '套料切割',
  children: [{
      path: 'cutting',
      component: 'Layout',
      hidden: false,
      name: 'nestCutting',
      alwaysShow: false,
      redirect: '/cutting/nest-task',
      meta: {
        title: '套料任务管理',
        icon: 'config-2',
        noCache: true
      },
      children: [{
          name: 'nestingTask',
          path: 'nesting-task',
          hidden: false,
          component: '/cutting/nest-task/index',
          meta: {
            title: '套料任务',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'nestingList',
          path: 'nesting-list',
          hidden: false,
          component: '/cutting/nest-list/index',
          meta: {
            title: '套料清单',
            icon: 'project',
            noCache: true
          }
        }
      ]
    },
    {
      path: '/nest-project-data',
      component: 'Layout',
      hidden: false,
      name: 'nestProjectData',
      alwaysShow: false,
      redirect: '/nest-project-data',
      meta: {
        title: '项目数据管理',
        icon: 'config-2',
        noCache: true
      },
      children: [{
        name: 'nestProjectData1',
        path: 'nest-project-data',
        hidden: false,
        component: '/cutting/nest-project-data/index',
        meta: {
          title: '项目数据',
          icon: 'project',
          noCache: true
        }
      }]
    },
    {
      path: '/cutting/cutting-task',
      component: 'Layout',
      hidden: false,
      name: 'cuttingTask',
      alwaysShow: false,
      redirect: '/cutting/cutting-task',
      meta: {
        title: '切割任务管理',
        icon: 'config-2',
        noCache: true
      },
      children: [{
        name: 'cuttingTask',
        path: 'cutting-task',
        hidden: false,
        component: '/cutting/cutting-task/index',
        meta: {
          title: '切割任务',
          icon: 'project',
          noCache: true
        }
      }]
    },
    {
      path: '/cutting/project-list',
      component: 'Layout',
      hidden: false,
      name: 'projectList',
      alwaysShow: false,
      redirect: '/cutting/project-list',
      meta: {
        title: '项目清单管理',
        icon: 'config-2',
        noCache: true
      },
      children: [{
        name: 'projectList',
        path: 'project-list',
        hidden: false,
        component: '/cutting/project-list/index',
        meta: {
          title: '项目清单',
          icon: 'project',
          noCache: true
        }
      }]
    },
    {
      path: '/cutting/machine-configuration',
      component: 'Layout',
      hidden: false,
      name: 'machineConfiguration',
      alwaysShow: false,
      redirect: '/cutting/machine-configuration',
      meta: {
        title: '机器配置管理  ',
        icon: 'config-2',
        noCache: true
      },
      children: [{
        name: 'machineConfiguration',
        path: 'machine-configuration',
        hidden: false,
        component: '/cutting/machine-configuration/index',
        meta: {
          title: '机器配置',
          icon: 'project',
          noCache: true
        }
      }]
    }
  ]
}
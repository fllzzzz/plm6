// 路由：套料切割
export default {
  id: 886,
  name: '套料切割',
  children: [{
    path: '/cutting',
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
        title: '套料工单',
        icon: 'project',
        noCache: true
      }
    }
    ]
  },
  // {
  //   path: '/cutting/cutting-task',
  //   component: 'Layout',
  //   hidden: false,
  //   name: 'cuttingTask',
  //   alwaysShow: false,
  //   redirect: '/cutting/cutting-task',
  //   meta: {
  //     title: '121',
  //     icon: 'config-2',
  //     noCache: true
  //   },
  //   children: [{
  //     name: 'cuttingTask',
  //     path: 'cutting-task',
  //     hidden: false,
  //     component: '/cutting/cutting-task/index',
  //     meta: {
  //       title: '1',
  //       icon: 'project',
  //       noCache: true
  //     }
  //   }
  //   ]
  // },
  {
    path: '/cutting/nest-work-list',
    component: 'Layout',
    hidden: false,
    name: 'cuttingWorkList',
    alwaysShow: false,
    redirect: '/cutting/nest-work-list',
    meta: {
      title: '套料工单管理',
      icon: 'config-2',
      noCache: true
    },
    children: [{
      name: 'nestWorkList',
      path: 'nest-work-list',
      hidden: false,
      component: '/cutting/nest-work-list/index',
      meta: {
        title: '套料工单',
        icon: 'project',
        noCache: true
      }
    }]
  },
  {
    path: '/cutting/cutting-work-order',
    component: 'Layout',
    hidden: false,
    name: 'cuttingWorkOrder',
    alwaysShow: false,
    redirect: '/cutting/cutting-work-order',
    meta: {
      title: '切割工单管理',
      icon: 'config-2',
      noCache: true
    },
    children: [{
      name: 'cuttingScheduling',
      path: 'cutting-scheduling',
      hidden: false,
      component: '/cutting/cutting-work-order/cutting-scheduling/index',
      meta: {
        title: '切割排产',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'cuttingReport',
      path: 'cutting-report',
      hidden: false,
      component: '/cutting/cutting-work-order/cutting-report/index',
      meta: {
        title: '切割报表',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'equipmentMonitoring',
      path: 'equipment-monitoring',
      hidden: false,
      component: '/cutting/cutting-work-order/equipment-monitoring/index',
      meta: {
        title: '设备监控',
        icon: 'project',
        noCache: true
      }
    }

    ]
  },
  // {
  //   path: '/cutting/project-list',
  //   component: 'Layout',
  //   hidden: false,
  //   name: 'projectList',
  //   alwaysShow: false,
  //   redirect: '/cutting',
  //   meta: {
  //     title: '切割工单管理(t)',
  //     icon: 'config-2',
  //     noCache: true
  //   },
  //   children: [
  //     {
  //       name: 'projectList',
  //       path: 'project-list',
  //       hidden: false,
  //       component: '/cutting/project-list/index',
  //       meta: {
  //         title: '切割排产',
  //         icon: 'project',
  //         noCache: true
  //       }
  //     }
  //   ]
  // },
  {
    path: '/cutting/machine',
    component: 'Layout',
    hidden: false,
    name: 'machine',
    alwaysShow: false,
    redirect: '/cutting/machine',
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

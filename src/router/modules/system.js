// 路由：系统管理
export default {
  id: -2,
  name: '系统管理',
  children: [
    {
      path: '/system',
      component: 'Layout',
      hidden: false,
      name: 'System',
      alwaysShow: true,
      redirect: '/system/dict',
      meta: { title: '系统配置', icon: 'system-gray', noCache: true },
      children: [
        {
          name: 'Dict',
          path: 'dict',
          hidden: false,
          component: '/system/dict/index',
          meta: { title: '字典管理', icon: 'dictionary', noCache: true }
        },
        {
          name: 'Menu',
          path: 'menu',
          hidden: false,
          component: '/system/menu/index',
          meta: { title: '菜单管理', icon: 'menu', noCache: true }
        },
        {
          name: 'PermissionType',
          path: 'permission-type',
          hidden: false,
          component: '/system/permission-type/index',
          meta: { title: '权限类型管理', icon: 'list', noCache: true }
        }
      ]
    },
    {
      path: 'member-manage',
      component: 'Layout',
      hidden: false,
      name: 'memberManage',
      alwaysShow: false,
      redirect: '/member-manage/dept',
      meta: { title: '人员管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'User',
          path: 'user',
          hidden: false,
          component: '/system/member-manage/user/index',
          meta: { title: '用户配置', icon: 'user', noCache: true }
        },
        {
          name: 'Dept',
          path: 'dept',
          hidden: false,
          component: '/system/member-manage/dept/index',
          meta: { title: '部门配置', icon: 'dept', noCache: true }
        },
        {
          name: 'Job',
          path: 'job',
          hidden: false,
          component: '/system/member-manage/job/index',
          meta: { title: '岗位配置', icon: 'Steve-Jobs', noCache: true }
        },
        {
          name: 'Role',
          path: 'role',
          hidden: false,
          component: '/system/member-manage/role/index',
          meta: { title: '角色配置', icon: 'role', noCache: true }
        }
      ]
    }
  ]
}
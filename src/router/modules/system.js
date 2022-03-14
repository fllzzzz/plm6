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
      meta: { title: '系统配置', icon: 'system', noCache: true },
      children: [
        {
          name: 'Dict',
          path: 'dict',
          hidden: false,
          component: '/system/dict/index',
          meta: { title: '字典管理', icon: 'project', noCache: true }
        },
        {
          name: 'Menu',
          path: 'menu',
          hidden: false,
          component: '/system/menu/index',
          meta: { title: '菜单管理', icon: 'project', noCache: true }
        },
        {
          name: 'PermissionType',
          path: 'permission-type',
          hidden: false,
          component: '/system/permission-type/index',
          meta: { title: '权限类型管理', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}

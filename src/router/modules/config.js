// 路由：配置管理
export default {
  id: 2,
  name: '配置管理',
  children: [
    {
      path: '/classification-manage',
      component: 'Layout',
      hidden: false,
      name: 'ClassificationManage',
      alwaysShow: false,
      redirect: '/config-manage/classification-manage/classification-config',
      meta: { title: '科目管理', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'ClassificationConfig',
          path: 'classification-config',
          hidden: false,
          component: '/config-manage/classification-manage/classification-config/index',
          meta: { title: '科目配置', icon: 'project', noCache: true }
        }
        // {
        //   name: 'SpecificationConfig',
        //   path: 'specification-config',
        //   hidden: false,
        //   component: '/classification-manage/specification-config/index',
        //   meta: { title: '规格配置', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'UnitConfig',
        //   path: 'unit-config',
        //   hidden: false,
        //   component: '/classification-manage/unit-config/index',
        //   meta: { title: '计量配置', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'OutboundMethod',
        //   path: 'outbound-method',
        //   hidden: false,
        //   component: '/classification-manage/outbound-method/index',
        //   meta: { title: '出库方式', icon: 'project', noCache: true }
        // }
        // {
        //   name: 'InstallationMethod',
        //   path: 'installation-method',
        //   hidden: false,
        //   component: '/classification-manage/installation-method/index',
        //   meta: { title: '安装方式', icon: 'project', noCache: true }
        // }
      ]
    },
    {
      path: '/wms-config',
      component: 'Layout',
      hidden: false,
      name: 'WMSConfig',
      alwaysShow: false,
      redirect: '/config-manage/wms/base',
      meta: { title: 'WMS配置管理', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'WmsBaseConfig',
          path: 'base',
          hidden: false,
          component: '/config-manage/wms/base/index',
          meta: { title: '基础配置', icon: 'project', noCache: true }
        }
        // {
        //   name: 'InventoryWarning',
        //   path: 'inventory-warning',
        //   hidden: false,
        //   component: '/wms-config/inventory-warning/index',
        //   meta: { title: '库存预警', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'UnitConfig',
        //   path: 'unit-config',
        //   hidden: false,
        //   component: '/wms-config/unit-config/index',
        //   meta: { title: '单位配置', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'MesConfigFactory',
        //   path: 'factory',
        //   hidden: false,
        //   component: '/wms-config/factory/index',
        //   meta: { title: '工厂管理', icon: 'factory', noCache: true }
        // },
        // {
        //   name: 'ConfigWarehouseSetting',
        //   path: 'warehouse-setting',
        //   hidden: false,
        //   component: '/wms-config/warehouse-setting/index',
        //   meta: { title: '仓库位设置', icon: 'Steve-Jobs', noCache: true }
        // },
        // {
        //   name: 'ConfigSectionSteelLibrary',
        //   path: 'profile-spec',
        //   hidden: false,
        //   component: '/wms-config/profile-spec/index',
        //   meta: { title: '型材库', icon: 'warehouse', noCache: true }
        // }
      ]
    }
  ]
}

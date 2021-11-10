// 路由：WMS
export default {
  id: 475,
  name: 'WMS',
  children: [
    {
      path: '/wms/inventory-warning-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplierManage',
      alwaysShow: false,
      redirect: '/wms/inventory-warning',
      meta: { title: '供应商管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'InventoryWarning',
          path: 'inventory-warning',
          hidden: false,
          component: '/wms/inventory-warning/index',
          meta: { title: '库存预警', icon: 'project', noCache: true }
        }
      ]
    }
    // {
    //   name: 'InventoryWarning',
    //   path: 'inventory-warning',
    //   hidden: false,
    //   component: '/wms-config/inventory-warning/index',
    //   meta: { title: '库存预警', icon: 'project', noCache: true }
    // },
    // {
    //   path: '/wms/prepares-materials',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'WmsPreparesMaterials',
    //   alwaysShow: false,
    //   redirect: '/wms/prepares-materials/custom',
    //   meta: { title: '备料管理', icon: 'contract', onCache: true },
    //   children: [
    //     {
    //       name: 'WmsPreparesMaterialsCustom',
    //       path: 'custom',
    //       hidden: false,
    //       component: '/wms/prepares-materials/custom/index',
    //       meta: { title: '备料定制', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'WmsPreparesMaterialsTrack',
    //       path: 'track',
    //       hidden: false,
    //       component: '/wms/prepares-materials/track/index',
    //       meta: { title: '备料跟踪', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'WmsFreezeMaterialManage',
    //       path: 'freeze-manage',
    //       hidden: false,
    //       component: '/wms/warehouse-management/freeze-manage',
    //       meta: { title: '冻结管理', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/wms',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'SupplierManage',
    //   alwaysShow: false,
    //   redirect: '/wms/supplier/manage',
    //   meta: { title: '供应商管理', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'PurchaseSupplier',
    //       path: 'supplier',
    //       hidden: false,
    //       component: '/wms/supplier/manage/index',
    //       meta: { title: '供应商列表', icon: 'supplier', noCache: true }
    //     }
    //   ]
    // }
  ]
}

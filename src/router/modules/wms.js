// 路由：WMS
export default {
  id: 475,
  name: 'WMS',
  children: [
    {
      path: '/wms/inbound-application',
      component: 'Layout',
      hidden: false,
      name: 'WMSInboundApplication',
      alwaysShow: false,
      redirect: '/wms/inbound-application/steel/index',
      meta: { title: '入库办理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelInboundApplication',
          path: 'steel',
          hidden: false,
          component: '/wms/inbound-application/steel/index',
          meta: { title: '钢材入库', icon: 'project', noCache: true }
        },
        {
          name: 'auxMatInboundApplication',
          path: 'auxiliary-material',
          hidden: false,
          component: '/wms/inbound-application/auxiliary-material/index',
          meta: { title: '辅材入库', icon: 'project', noCache: true }
        },
        {
          name: 'gasInboundApplication',
          path: 'gas',
          hidden: false,
          component: '/wms/inbound-application/gas/index',
          meta: { title: '气体入库', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/purchase-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSPurchaseOrder',
      alwaysShow: false,
      redirect: '/wms/purchase-order',
      meta: { title: '采购订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/wms/purchase-order/index',
          meta: { title: '采购订单', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/logistics-manage',
      component: 'Layout',
      hidden: false,
      name: 'WMSLogisticsOrder',
      alwaysShow: false,
      redirect: '/wms/logistics-order',
      meta: { title: '物流订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'LogisticsOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/wms/logistics-order/index',
          meta: { title: '物流订单', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/wms/inventory-warning-manage',
      component: 'Layout',
      hidden: false,
      name: 'InventoryManage',
      alwaysShow: false,
      redirect: '/wms/inventory-warning',
      meta: { title: '库存管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'InventoryWarning',
          path: 'inventory-warning',
          hidden: false,
          component: '/wms/inventory-warning/index',
          meta: { title: '库存预警', icon: 'project', noCache: true }
        }
      ]
    },
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
    {
      path: '/wms/supplier',
      component: 'Layout',
      hidden: false,
      name: 'SupplierManage',
      alwaysShow: false,
      redirect: '/wms/supplier/manage',
      meta: { title: '供应商管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseSupplier',
          path: 'manage',
          hidden: false,
          component: '/wms/supplier/manage/index',
          meta: { title: '供应商列表', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
